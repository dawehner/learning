<?php
declare(strict_types=1);

namespace Soong\Data;

/**
 * Basic implementation of a Record factory.
 */
class RecordFactory implements \Soong\Contracts\Data\RecordFactory
{

    /**
     * @inheritdoc
     */
    public function create(array $data = []): \Soong\Contracts\Data\Record
    {
        return new Record($data);
    }
}
