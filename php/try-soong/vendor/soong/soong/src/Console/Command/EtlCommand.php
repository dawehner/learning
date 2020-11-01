<?php
declare(strict_types=1);

namespace Soong\Console\Command;

use Noodlehaus\Config;
use Soong\Contracts\Extractor\Extractor;
use Soong\Contracts\KeyMap\KeyMap;
use Soong\Contracts\Loader\Loader;
use Soong\Data\RecordFactory;
use Soong\Filter\Select;
use Soong\Task\TaskPipeline;
use Symfony\Component\Console\Command\Command;
use Symfony\Component\Console\Input\InputArgument;
use Symfony\Component\Console\Input\InputOption;

/**
 * Base class for all Soong console commands.
 */
class EtlCommand extends Command
{

    /**
     * The task pipeline used in executing the command.
     *
     * @var TaskPipeline $pipeline
     */
    protected $pipeline;

    /**
     * Configure the "tasks" command argument for one or more values.
     *
     * @param bool $required
     *   TRUE if an explicit task is required, FALSE otherwise.
     *
     * @return InputArgument
     *   The configured command argument.
     */
    protected function tasksArgument(bool $required = true): InputArgument
    {
        $required = $required ? InputArgument::REQUIRED : InputArgument::OPTIONAL;
        return new InputArgument(
            'tasks',
            InputArgument::IS_ARRAY | $required,
            'List of task IDs to process'
        );
    }

    /**
     * Configure the "directory" option to be required with one or more values.
     *
     * @return InputOption
     *   The configured command option.
     */
    protected function directoryOption(): InputOption
    {
        return new InputOption(
            'directory',
            null,
            InputOption::VALUE_IS_ARRAY | InputOption::VALUE_REQUIRED,
            'List of directories containing tasks to migrate',
            ['config']
        );
    }

    /**
     * Configure the "select" option.
     *
     * @return InputOption
     *   The configured command option.
     */
    protected function selectOption(): InputOption
    {
        return new InputOption(
            'select',
            null,
            InputOption::VALUE_IS_ARRAY | InputOption::VALUE_OPTIONAL,
            'List of property name=value criteria'
        );
    }

    /**
     * Configure the "limit" option.
     *
     * @return InputOption
     *   The configured command option.
     */
    protected function limitOption(): InputOption
    {
        return new InputOption(
            'limit',
            null,
            InputOption::VALUE_OPTIONAL,
            'Maximum number of records to process'
        );
    }

    /**
     * Obtain all task configuration contained in the specified directories.
     *
     * @param string[] $directoryNames
     *   List of directories containing task configuration.
     * @param array $options
     *   List of command-line options.
     */
    protected function loadConfiguration(
        array $directoryNames,
        array $options = []
    ): void {
        $this->pipeline = new TaskPipeline();
        $recordFactory = new RecordFactory();
        foreach ($directoryNames as $directoryName) {
            $conf = Config::load($directoryName);
            foreach ($conf->all() as $id => $configuration) {
                $taskClass = $configuration['class'];
                $taskConfiguration = $configuration['configuration'];
                $taskConfiguration['record_factory'] = $recordFactory;
                $taskConfiguration['extract']['configuration']['record_factory'] =
                    $recordFactory;
                $taskConfiguration['extract'] = $this->getExtractor(
                    $taskConfiguration['extract'],
                    $options
                );
                if (isset($taskConfiguration['transform'])) {
                    $taskConfiguration['transform'] =
                        $this->getTransform($taskConfiguration['transform']);
                }
                $taskConfiguration['load'] = $this->getLoader($taskConfiguration['load']);
                if (isset($taskConfiguration['key_map'])) {
                    $taskConfiguration['key_map'] = $this->getKeyMap(
                        $taskConfiguration['key_map'],
                        $taskConfiguration['extract']->getKeyProperties(),
                        $taskConfiguration['load']->getKeyProperties()
                    );
                }
                // Inject the pipeline into each task so it knows its parent.
                $task = new $taskClass($taskConfiguration +
                    [
                        'pipeline' => $this->pipeline,
                    ]
                );
                $this->pipeline->addTask($id, $task);
            }
        }
    }

    /**
     * Construct an Extractor instance.
     *
     * @param array $configuration
     *   The extractor's configuration.
     * @param array $options
     *   Runtime options which may affect the configuration.
     *
     * @return \Soong\Contracts\Extractor\Extractor|null
     */
    protected function getExtractor(
        array $configuration,
        array $options
    ): ?Extractor {
        /** @var \Soong\Contracts\Extractor\Extractor $extractorClass */
        $extractorClass = $configuration['class'];
        $extractorConfiguration = $configuration['configuration'];
        if (!empty($options['select'])) {
            // Each expression arrives in the form "$name$op$value" = we need to
            // turn that into an array [$name, $op, $value].
            $criteria = [];
            // Note that if '=' is before '==' in the operator array, 'a==b'
            // will be parsed as 'a', '=', '=b'. To prevent this, make sure the
            // operators are sorted longest first.
            $operatorList = Select::OPERATORS;
            usort($operatorList, function ($a, $b) {
                return $b <=> $a;
            });
            $operatorExpression = implode('|', $operatorList);
            foreach ($options['select'] as $expression) {
                if (!preg_match(
                    "/(.*?)($operatorExpression)(.*)/",
                    $expression,
                    $matches
                )) {
                    throw new \InvalidArgumentException("--select: Invalid expression $expression");
                }
                $criteria[] = [$matches[1], $matches[2], $matches[3]];
            }
            $extractorConfiguration['filters'][] = [
                'class' => 'Soong\Filter\Select',
                'configuration' => [
                    'criteria' => $criteria,
                ],
            ];
        }
        // Replace filter configuration with actual instances.
        if (!empty($extractorConfiguration['filters'])) {
            foreach ($extractorConfiguration['filters'] as $key => $filter) {
                $extractorConfiguration['filters'][$key] =
                    new $filter['class']($filter['configuration']);
            }
        }
        $extractor = new $extractorClass($extractorConfiguration);
        return $extractor;
    }

    /**
     * Construct the transformation pipeline.
     *
     * @param array $configuration
     *   A list of RecordTransformer configurations.
     *
     * @return \Soong\Contracts\Transformer\RecordTransformer[]
     */
    protected function getTransform(array $configuration): array
    {
        /** @var \Soong\Contracts\Transformer\RecordTransformer[] $recordTransformers */
        $recordTransformers = [];
        foreach ($configuration as $recordTransformer) {
            $recordTransformerClass = $recordTransformer['class'];
            $recordTransformerConfiguration = $recordTransformer['configuration'];
            if ('Soong\Transformer\Record\PropertyMapper' === $recordTransformerClass) {
                $recordTransformerConfiguration['property_map'] = [];
                foreach ($recordTransformer['configuration']['property_map'] as $property => $transformerList) {
                    // Shortcut for directly mapping properties.
                    if (is_string($transformerList)) {
                        $sourceProperty = $transformerList;
                        $transformerList = [
                            [
                                'class' => 'Soong\Transformer\Property\Copy',
                                'source_property' => $sourceProperty,
                            ],
                        ];
                    }
                    foreach ($transformerList as $transformerStuff) {
                        $transformerConfiguration = $transformerStuff['configuration'] ?? [];
                        // @todo Better way to provide context
                        $transformerConfiguration['pipeline'] = $this->pipeline;

                        /** @var \Soong\Contracts\Transformer\PropertyTransformer $transformerClass */
                        $transformerClass = $transformerStuff['class'];
                        $recordTransformerConfiguration['property_map'][$property][] = [
                            'transformer' => new $transformerClass($transformerConfiguration),
                            'source_property' => $transformerStuff['source_property'] ?? null,
                        ];
                    }
                }
            }
            $recordTransformers[] = new $recordTransformerClass($recordTransformerConfiguration);
        }

        return $recordTransformers;
    }

    /**
     * Construct the loader.
     *
     * @param array $configuration
     *   Configuration of the loader.
     *
     * @return \Soong\Contracts\Loader\Loader|null
     */
    protected function getLoader(array $configuration): ?Loader
    {
        /** @var \Soong\Contracts\Loader\Loader $loaderClass */
        $loaderClass = $configuration['class'];
        $loader = new $loaderClass($configuration['configuration']);
        return $loader;
    }

    /**
     * Construct the key map.
     *
     * @param array $configuration
     *   Configuration of the key map.
     *
     * @return \Soong\Contracts\KeyMap\KeyMap|null
     */
    protected function getKeyMap(
        array $configuration,
        array $extractorKeys,
        array $loaderKeys
    ): ?KeyMap {
        $keyMapConfiguration = $configuration['configuration'] ?? [];
        $keyMapConfiguration = array_merge(
            ['extractor_keys' => $extractorKeys],
            $keyMapConfiguration
        );
        $keyMapConfiguration = array_merge(
            ['loader_keys' => $loaderKeys],
            $keyMapConfiguration
        );
        /** @var \Soong\Contracts\KeyMap\KeyMap $keyMapClass */
        $keyMapClass = $configuration['class'];
        $keyMap = new $keyMapClass($keyMapConfiguration);
        return $keyMap;
    }
}
