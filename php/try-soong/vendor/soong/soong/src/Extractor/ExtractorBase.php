<?php
declare(strict_types=1);

namespace Soong\Extractor;

use Soong\Configuration\OptionsResolverComponent;
use Soong\Contracts\Extractor\Extractor;

/**
 * Common implementation details for extractors.
 */
abstract class ExtractorBase extends OptionsResolverComponent implements Extractor
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['properties'] = [
            'required' => true,
            'default_value' => [],
            'allowed_types' => 'array',
        ];
        $options['key_properties'] = [
            'required' => true,
            'default_value' => [],
            'allowed_types' => 'array',
        ];
        $options['filters'] = [
            'required' => false,
            'default_value' => [],
            'allowed_types' => 'Soong\Contracts\Filter\Filter[]',
        ];
        $options['record_factory'] = [
            'required' => true,
            'allowed_types' => 'Soong\Contracts\Data\RecordFactory',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function getProperties(): array
    {
        return $this->getConfigurationValue('properties');
    }

    /**
     * @inheritdoc
     */
    public function getKeyProperties(): array
    {
        return $this->getConfigurationValue('key_properties');
    }

    /**
     * @inheritdoc
     */
    public function extractFiltered() : iterable
    {
        foreach ($this->extractAll() as $record) {
            $yield = true;
            /** @var \Soong\Contracts\Filter\Filter $filter */
            foreach ($this->getConfigurationValue('filters') as $filter) {
                if (!$filter->filter($record)) {
                    $yield = false;
                    break;
                }
            }
            if ($yield) {
                yield $record;
            }
        }
    }
}
