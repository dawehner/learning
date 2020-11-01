<?php
declare(strict_types=1);

namespace Soong\Contracts\Exception;

/**
 * Base for all exceptions thrown by/about record transformers.
 */
class RecordTransformerException extends \RuntimeException implements TransformerException
{

}
