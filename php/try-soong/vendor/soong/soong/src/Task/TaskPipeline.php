<?php
declare(strict_types=1);

namespace Soong\Task;

use Soong\Contracts\Exception\ComponentNotFound;
use Soong\Contracts\Exception\DuplicateTask;
use Soong\Contracts\Task\Task;
use Soong\Contracts\Task\TaskPipeline as TaskPipelineInterface;

/**
 * Basic base class for migration tasks.
 */
class TaskPipeline implements TaskPipelineInterface
{

    /**
     * @internal
     *
     * All known tasks, keyed by id.
     *
     * @var Task[] $tasks
     */
    protected $tasks = [];

    /**
     * @inheritdoc
     */
    public function addTask(string $id, Task $task) : void
    {
        if (!empty($this->tasks[$id])) {
            throw new DuplicateTask("Task $id already exists.");
        }
        $this->tasks[$id] = $task;
    }

    /**
     * @inheritdoc
     */
    public function getTask(string $id): Task
    {
        if (empty($this->tasks[$id])) {
            throw new ComponentNotFound("Task $id not found.");
        }
        return $this->tasks[$id];
    }

    /**
     * @inheritdoc
     */
    public function getAllTasks(): array
    {
        return $this->tasks;
    }
}
