<?php
declare(strict_types=1);

namespace Soong\Transformer\Record;

use Soong\Configuration\OptionsResolverComponent;
use Soong\Contracts\Data\Record;
use Soong\Contracts\Transformer\RecordTransformer;

/**
 * Shortcut for direct copying of properties.
 */
class Copy extends OptionsResolverComponent implements RecordTransformer
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        // @todo Add validation that only one of these options is present.
        $options['include'] = [
            'allowed_types' => 'string[]',
        ];
        $options['exclude'] = [
            'allowed_types' => 'string[]',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function transform(
        Record $sourceData,
        Record $incomingResultData
    ): Record {
        // Build upon what's already been populated.
        $resultData = clone $incomingResultData;
        // Determine the specific properties to copy.
        $propertyNames = array_keys($sourceData->toArray());
        if (!empty($includedProperties = $this->getConfigurationValue('include'))) {
            $propertyNames = array_intersect($propertyNames, $includedProperties);
        } elseif (!empty($excludedProperties = $this->getConfigurationValue('exclude'))) {
            $propertyNames = array_filter(
                $propertyNames,
                function ($name) use ($excludedProperties) {
                    return !in_array($name, $excludedProperties);
                }
            );
        }
        foreach ($propertyNames as $sourcePropertyName) {
            $resultData->setPropertyValue(
                $sourcePropertyName,
                $sourceData->getPropertyValue($sourcePropertyName)
            );
        }
        return $resultData;
    }
}
