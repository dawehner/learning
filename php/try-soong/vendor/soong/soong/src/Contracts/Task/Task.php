<?php
declare(strict_types=1);

namespace Soong\Contracts\Task;

use Soong\Contracts\Configuration\ConfigurableComponent;

/**
 * Interface for tasks implementing operations.
 */
interface Task extends ConfigurableComponent
{

    /**
     * Execute the named operation for the task.
     *
     * @param string $operation
     *   Name of the operation, which is a method on the class.
     * @param array $options
     *   List of options affecting execution of the operation.
     *
     * @throws \BadMethodCallException
     */
    public function execute(string $operation, array $options = []) : void;
}
