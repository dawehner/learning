<?php
declare(strict_types=1);

namespace Soong\Contracts\Exception;

/**
 * Thrown when an attempting to add a task with an already existing ID.
 */
class DuplicateTask extends \RuntimeException implements TaskException
{

}
