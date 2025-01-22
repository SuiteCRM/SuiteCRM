<?php
namespace Api\V8\Factory;

use Symfony\Component\Validator\Validator\ValidatorInterface;

#[\AllowDynamicProperties]
class ValidatorFactory
{
    /**
     * @var ValidatorInterface
     */
    protected $validator;

    /**
     * @param ValidatorInterface $validator
     */
    public function __construct(ValidatorInterface $validator)
    {
        $this->validator = $validator;
    }

    /**
     * @param \Symfony\Component\Validator\Constraint[] $constraints
     * @param boolean $allowNull
     *
     * @return \Closure
     */
    public function createClosure(array $constraints, $allowNull = false)
    {
        return function ($value) use ($constraints, $allowNull) {
            if ($allowNull && $value === null) {
                return true;
            }

            $violations = $this->validator->validate($value, $constraints);

            return !$violations->count();
        };
    }

    /**
     * @param \Symfony\Component\Validator\Constraint[] $constraints
     * @param boolean $allowNull
     *
     * @return \Closure
     */
    public function createClosureForIterator(array $constraints, $allowNull = false)
    {
        return function ($value) use ($constraints, $allowNull) {
            if ($allowNull && $value === null) {
                return true;
            }

            if (!is_array($value) && !$value instanceof \Iterator) {
                return false;
            }

            foreach ($value as $v) {
                if ($this->validator->validate($v, $constraints)->count()) {
                    return false;
                }
            }

            return true;
        };
    }
}
