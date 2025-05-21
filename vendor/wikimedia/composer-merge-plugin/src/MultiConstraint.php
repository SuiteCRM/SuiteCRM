<?php

/**
 * This file is part of the Composer Merge plugin.
 *
 * Copyright (C) 2021 Bryan Davis, Wikimedia Foundation, and contributors
 *
 * This software may be modified and distributed under the terms of the MIT
 * license. See the LICENSE file for details.
 */

namespace Wikimedia\Composer\Merge\V2;

use Composer\Semver\Constraint\ConstraintInterface;
use Composer\Semver\Constraint\EmptyConstraint;
use Composer\Semver\Constraint\MultiConstraint as SemverMultiConstraint;
use function count;

/**
 * Adapted from Composer's v2 MultiConstraint::create for Composer v1
 * @link https://github.com/composer/semver/blob/3.2.4/src/Constraint/MultiConstraint.php
 * @author Chauncey McAskill <chauncey@mcaskill.ca>
 */
class MultiConstraint extends SemverMultiConstraint
{
    /**
     * Tries to optimize the constraints as much as possible, meaning
     * reducing/collapsing congruent constraints etc.
     * Does not necessarily return a MultiConstraint instance if
     * things can be reduced to a simple constraint
     *
     * @param ConstraintInterface[] $constraints A set of constraints
     * @param bool                  $conjunctive Whether the constraints should be treated as conjunctive or disjunctive
     *
     * @return ConstraintInterface
     */
    public static function create(array $constraints, $conjunctive = true)
    {
        if (count($constraints) === 0) {
            // EmptyConstraint only exists in composer 1.x. Configure as to run phan against composer 2.x
            // @phan-suppress-next-line PhanTypeMismatchReturn, PhanUndeclaredClassMethod
            return new EmptyConstraint();
        }

        if (count($constraints) === 1) {
            return $constraints[0];
        }

        $optimized = self::optimizeConstraints($constraints, $conjunctive);
        if ($optimized !== null) {
            list($constraints, $conjunctive) = $optimized;
            if (count($constraints) === 1) {
                return $constraints[0];
            }
        }

        return new self($constraints, $conjunctive);
    }

    /**
     * @return array|null
     */
    private static function optimizeConstraints(array $constraints, $conjunctive)
    {
        // parse the two OR groups and if they are contiguous we collapse
        // them into one constraint
        // [>= 1 < 2] || [>= 2 < 3] || [>= 3 < 4] => [>= 1 < 4]
        if (!$conjunctive) {
            $left = $constraints[0];
            $mergedConstraints = [];
            $optimized = false;
            for ($i = 1, $l = count($constraints); $i < $l; $i++) {
                $right = $constraints[$i];
                if ($left instanceof SemverMultiConstraint
                    && $left->conjunctive
                    && $right instanceof SemverMultiConstraint
                    && $right->conjunctive
                    && count($left->constraints) === 2
                    && count($right->constraints) === 2
                    && ($left0 = (string) $left->constraints[0])
                    && $left0[0] === '>' && $left0[1] === '='
                    && ($left1 = (string) $left->constraints[1])
                    && $left1[0] === '<'
                    && ($right0 = (string) $right->constraints[0])
                    && $right0[0] === '>' && $right0[1] === '='
                    && ($right1 = (string) $right->constraints[1])
                    && $right1[0] === '<'
                    && substr($left1, 2) === substr($right0, 3)
                ) {
                    $optimized = true;
                    $left = new MultiConstraint(
                        [
                            $left->constraints[0],
                            $right->constraints[1],
                        ],
                        true
                    );
                } else {
                    $mergedConstraints[] = $left;
                    $left = $right;
                }
            }
            if ($optimized) {
                $mergedConstraints[] = $left;
                return [$mergedConstraints, false];
            }
        }

        // TODO: Here's the place to put more optimizations

        return null;
    }
}
// vim:sw=4:ts=4:sts=4:et:
