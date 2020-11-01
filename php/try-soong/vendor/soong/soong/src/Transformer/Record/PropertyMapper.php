<?php
declare(strict_types=1);

namespace Soong\Transformer\Record;

use Soong\Configuration\OptionsResolverComponent;
use Soong\Contracts\Data\Record;
use Soong\Contracts\Transformer\RecordTransformer;

/**
 * Populates records property-by-property.
 */
class PropertyMapper extends OptionsResolverComponent implements RecordTransformer
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['property_map'] = [
            'required' => true,
            'allowed_types' => 'array',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function transform(Record $sourceData, Record $incomingResultData): Record
    {
        $resultData = clone $incomingResultData;
        foreach ($this->getConfigurationValue('property_map') as $resultProperty => $transformerList) {
            // Go through each transformer for this result property.
            $currentData = null;
            foreach ($transformerList as $transformerEntry) {
                /** @var \Soong\Contracts\Transformer\PropertyTransformer $transformer */
                $transformer = $transformerEntry['transformer'];
                if (isset($transformerEntry['source_property'])) {
                    $currentData = $sourceData->getPropertyValue($transformerEntry['source_property']);
                }
                $currentData = $transformer->transform($currentData);
            }
            $resultData->setPropertyValue($resultProperty, $currentData);
        }
        return $resultData;
    }
}
