<?php
declare(strict_types=1);

namespace Soong\Contracts\Filter;

use Soong\Contracts\Configuration\ConfigurableComponent;
use Soong\Contracts\Data\Record;

/**
 * Filters decide whether a Record should or should not be processed.
 */
interface Filter extends ConfigurableComponent
{

    /**
     * Decide whether a data record should be processed.
     *
     * @param \Soong\Contracts\Data\Record $record
     *   Record to examine.
     *
     * @return bool
     *   TRUE if the record should be processed, FALSE if it should be skipped.
     */
    public function filter(Record $record) : bool;
}
