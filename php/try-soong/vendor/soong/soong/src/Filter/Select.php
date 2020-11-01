<?php
declare(strict_types=1);

namespace Soong\Filter;

use Soong\Configuration\OptionsResolverComponent;
use Soong\Contracts\Data\Record;
use Soong\Contracts\Exception\UnrecognizedOperator;
use Soong\Contracts\Filter\Filter;

/**
 * Filter out records based on specific property values.
 *
 * One configuration option is supported, 'criteria', which consists of an array
 * of criterion arrays, each containing three values: the name of the property
 * whose value will be the first operand, one of the comparative operators
 * listed in Select::OPERATORS, and the value for the second operand. Some
 * examples:
 *
 * @code
 * // This filter accepts only data records whose id property is equal to 5.
 * $filter = new Select([
 *   'criteria' => [
 *     ['id', '==', 5],
 *   ],
 * ]);
 * @endcode
 *
 * @code
 * // This filter accepts only data records whose status property is equal to 1
 * // and whose last_name value is in the last half of the alphabet.
 * $filter = new Select([
 *   'criteria' => [
 *     ['status', '==', 1],
 *     ['last_name', '>=', 'N'],
 *   ],
 * ]);
 * @endcode
 */
class Select extends OptionsResolverComponent implements Filter
{

    /**
     * The comparison operators supported by this filter.
     */
    public const OPERATORS = [
        '=',    // Mapped to ==
        '==',
        '===',
        '!=',
        '<>',
        '!==',
        '<',
        '>',
        '<=',
        '>=',
    ];

    /**
     * @inheritdoc
     */
    public function filter(Record $record): bool
    {
        $criteria = $this->getConfigurationValue('criteria');
        foreach ($criteria as $criterion) {
            [$propertyName, $operator, $testValue] = $criterion;
            $propertyValue = $record->getPropertyValue($propertyName);
            switch ($operator) {
                case '=':
                case '==':
                    if (!($propertyValue == $testValue)) {
                        return false;
                    }
                    break;
                case '===':
                    if (!($propertyValue === $testValue)) {
                        return false;
                    }
                    break;
                case '!=':
                    if (!($propertyValue != $testValue)) {
                        return false;
                    }
                    break;
                case '<>':
                    if (!($propertyValue <> $testValue)) {
                        return false;
                    }
                    break;
                case '!==':
                    if (!($propertyValue !== $testValue)) {
                        return false;
                    }
                    break;
                case '<':
                    if (!($propertyValue < $testValue)) {
                        return false;
                    }
                    break;
                case '>':
                    if (!($propertyValue > $testValue)) {
                        return false;
                    }
                    break;
                case '<=':
                    if (!($propertyValue <= $testValue)) {
                        return false;
                    }
                    break;
                case '>=':
                    if (!($propertyValue >= $testValue)) {
                        return false;
                    }
                    break;
                default:
                    throw new UnrecognizedOperator("Select filter: unrecognized operator $operator");
            }
        }
        return true;
    }

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['criteria'] = [
            'required' => true,
            'allowed_types' => 'array[]',
        ];
        return $options;
    }
}
