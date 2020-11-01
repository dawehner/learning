<?php
declare(strict_types=1);

namespace Soong\Loader;

use Soong\Contracts\Data\Record;

/**
 * Loader for testing/debugging pipelines.
 */
class PrintR extends LoaderBase
{

    /**
     * @inheritdoc
     */
    public function load(Record $data) : void
    {
        print_r($data);
    }

    /**
     * @inheritdoc
     */
    public function getKeyProperties(): array
    {
        return [];
    }

    /**
     * @inheritdoc
     */
    public function getProperties(): array
    {
        return [];
    }

    /**
     * @inheritdoc
     */
    public function delete(array $key) : void
    {
        // @todo not supported
    }
}
