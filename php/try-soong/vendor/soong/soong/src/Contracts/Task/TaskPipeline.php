<?php
declare(strict_types=1);

namespace Soong\Contracts\Task;

/**
 * Interface for managing tasks as part of a pipeline.
 */
interface TaskPipeline
{

    /**
     * Retrieve the specified task.
     *
     * @param string $id
     *   ID of the task to retrieve.
     *
     * @return Task
     *   The specified task.
     *
     * @throws \Soong\Contracts\Exception\ComponentNotFound
     *   If the specified ID is not stored in the pipeline.
     */
    public function getTask(string $id) : Task;

    /**
     * Retrieve a list of all tasks.
     *
     * @return Task[]
     *   List of tasks, keyed by ID.
     */
    public function getAllTasks() : array;

    /**
     * Add an existing task object with the given task ID.
     *
     * @param string $id
     *   ID of the task to add.
     * @param Task $task
     *   Task object to add.
     * @return void
     *
     * @throws \Soong\Contracts\Exception\DuplicateTask
     */
    public function addTask(string $id, Task $task) : void;
}
