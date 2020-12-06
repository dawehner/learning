<?php
declare(strict_types=1);

namespace Soong\Contracts\Loader;

use Soong\Contracts\Configuration\ConfigurableComponent;
use Soong\Contracts\Data\Record;

/**
 * Loaders take one Record at a time and load them into a destination.
 */
interface Loader extends ConfigurableComponent
{

    /**
     * This needs to return disposition (success, failure) and key of result.
     *
     * @param Record $data
     *   Data to be loaded into the destination.
     *
     * @throws \Soong\Contracts\Exception\LoaderException
     */
    public function load(Record $data) : void;

    /**
     * List the properties available in records generated by this extractor.
     *
     * @return array
     *   Array of property names.
     */
    public function getProperties() : array;

    /**
     * List the properties which form a unique key for the extracted data.
     *
     * @return array
     *   Array keyed by property name, with value being an array containing
     *   a 'type' key whose value is the type of the property.
     */
    public function getKeyProperties() : array;

    /**
     * Remove a record which has been loaded from the destination.
     *
     * @param array $key
     *   Unique key of the destination record to be removed.
     *
     * @throws \Soong\Contracts\Exception\LoaderException
     */
    public function delete(array $key) : void;
}