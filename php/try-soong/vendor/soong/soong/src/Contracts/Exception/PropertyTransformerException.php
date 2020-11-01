<?php
declare(strict_types=1);

namespace Soong\Contracts\Exception;

/**
 * Base for all exceptions thrown by/about property transformers.
 */
class PropertyTransformerException extends \RuntimeException implements TransformerException
{

}
