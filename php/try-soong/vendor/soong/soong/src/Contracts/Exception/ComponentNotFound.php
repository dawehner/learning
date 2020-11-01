<?php
declare(strict_types=1);

namespace Soong\Contracts\Exception;

/**
 * Thrown when an attempt to discover and return a Soong component fails.
 */
class ComponentNotFound extends \RuntimeException implements SoongException
{

}
