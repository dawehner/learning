<?php
declare(strict_types=1);

namespace Soong\Contracts\Data;

/**
 * Factory for creating Record instances.
 */
interface RecordFactory
{

    /**
     * Create a record populated with a set of named data properties.
     *
     * @param array $data
     *   Associative array of property values, keyed by property name.
     */
    public function create(array $data = []) : Record;
}
