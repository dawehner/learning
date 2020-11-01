<?php
declare(strict_types=1);

namespace Soong\Transformer\Property;

use Soong\Contracts\Task\TaskPipeline;

/**
 * PropertyTransformer accepting a unique key from the extracted data and
 * looking up the key of any data loaded from it.
 *
 * Configuration:
 *   key_map:
 *     task_id: Unique identifier of the EtlTask which migrated the data.
 */
class KeyMapLookup extends PropertyTransformerBase
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['pipeline'] = [
            'required' => true,
            'allowed_types' => TaskPipeline::class,
        ];
        $options['key_map'] = [
            'required' => true,
            'allowed_types' => 'array',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function transform($data)
    {
        if (is_null($data)) {
            return null;
        }
        // @todo: Better way to inject key maps.
        $keyMapConfig = $this->getConfigurationValue('key_map');
        /** @var \Soong\Contracts\Task\TaskPipeline $pipeline */
        $pipeline = $this->getConfigurationValue('pipeline');
        /** @var \Soong\Contracts\Task\EtlTask $task */
        $task = $pipeline->getTask($keyMapConfig['task_id']);
        // @todo: Allow multiple key maps.
        $keyMap = $task->getKeyMap();
        $loadedKey = $keyMap->lookupLoadedKey([$data]);
        if (!empty($loadedKey)) {
            // @todo: Handle multi-value keys properly.
            return reset($loadedKey);
        }
        // @todo: Support creation of stubs when nothing found.
        return null;
    }
}
