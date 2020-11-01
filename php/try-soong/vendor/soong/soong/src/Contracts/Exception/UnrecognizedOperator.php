<?php
declare(strict_types=1);

namespace Soong\Contracts\Exception;

/**
 * Thrown when an unrecognized operator is passed to the Select filter.
 */
class UnrecognizedOperator extends \RuntimeException implements FilterException
{

}
