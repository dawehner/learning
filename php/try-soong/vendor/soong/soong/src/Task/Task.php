<?php
declare(strict_types=1);

namespace Soong\Task;

use Soong\Configuration\OptionsResolverComponent;
use Soong\Contracts\Task\Task as TaskInterface;

/**
 * Basic base class for migration tasks.
 */
class Task extends OptionsResolverComponent implements TaskInterface
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['pipeline'] = [
            'required' => true,
            'allowed_types' => 'Soong\Contracts\Task\TaskPipeline',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function execute(string $operation, array $options = []) : void
    {
        if (method_exists($this, $operation)) {
            $this->$operation($options);
        } else {
            throw new \BadMethodCallException("No $operation method exists.");
        }
    }
}
