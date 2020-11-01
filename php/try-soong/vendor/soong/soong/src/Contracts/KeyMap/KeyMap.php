<?php
declare(strict_types=1);

namespace Soong\Contracts\KeyMap;

use Soong\Contracts\Configuration\ConfigurableComponent;

/**
 * Represents the mapping of extracted keys to loaded keys.
 */
interface KeyMap extends ConfigurableComponent, \Countable
{

    /**
     * Persist the mapping of an extracted key to the corresponding loaded key.
     *
     * @param array $extractedKey
     *   Extracted key values, keyed by key names.
     * @param array $loadedKey
     *   Loaded key values, keyed by key names.
     *
     * @throws \Soong\Contracts\Exception\KeyMapException
     */
    public function saveKeyMap(array $extractedKey, array $loadedKey) : void;

    /**
     * Retrieve the loaded key corresponding to a given extracted key.
     *
     * @param array $extractedKey
     *   Extracted key values, keyed by key names.
     *
     * @return array
     *   Loaded key values, keyed by key names.
     */
    public function lookupLoadedKey(array $extractedKey) : array;

    /**
     * Retrieve any extracted keys mapped to a given loaded key.
     *
     * Note that multiple extracted keys may map to one loaded key - while
     * lookupLoadedKey returns a single key array, lookupExtractedKeys returns
     * an array of key arrays.
     *
     * @param array $loadedKey
     *   Loaded key values, keyed by key names.
     *
     * @return array[]
     *   Array of extracted keys, each of which is keyed by key names.
     */
    public function lookupExtractedKeys(array $loadedKey) : array;

    /**
     * Remove the mapping for a given extracted key from the map.
     *
     * @param array $extractedKey
     *   Extracted key values, keyed by key names.
     */
    public function delete(array $extractedKey) : void;

    /**
     * Iterate over the key map, generating the keys.
     *
     * @return iterable
     */
    public function iterate() : iterable;
}
