<?php
declare(strict_types=1);

namespace Soong\KeyMap;

use Soong\Configuration\OptionsResolverComponent;
use Soong\Contracts\KeyMap\KeyMap;

/**
 * Common implementation many/most key map implementations will need.
 */
abstract class KeyMapBase extends OptionsResolverComponent implements KeyMap
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['extractor_keys'] = [
            'required' => true,
            'default_value' => [],
            'allowed_types' => 'array',
        ];
        $options['loader_keys'] = [
            'required' => true,
            'default_value' => [],
            'allowed_types' => 'array',
        ];
        $options['hash'] = [
            'required' => true,
            'default_value' => 'sha256',
            'allowed_types' => 'string',
        ];
        return $options;
    }

    /**
     * Create a unique hash of the provided key values.
     *
     * The resulting hash should have a consistent length regardless of values.
     *
     * @param array $key
     *  An array of key values.
     *
     * @return string
     *  A string uniquely representing the key values.
     */
    protected function hashKeys(array $key) : string
    {
        // The type of a key value may differ between what's originally obtained
        // from the source and what's later read from the key map, changing the
        // serialized value and therefore the hash. Make sure every scalar in
        // the key array is a string before serializing.
        array_walk_recursive($key, function (&$value) {
            $value = strval($value);
        });
        $serialized = serialize(array_values($key));
        $hash = hash($this->getConfigurationValue('hash'), $serialized);
        return $hash;
    }
}
