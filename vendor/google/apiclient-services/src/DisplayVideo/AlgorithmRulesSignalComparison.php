<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\DisplayVideo;

class AlgorithmRulesSignalComparison extends \Google\Model
{
  /**
   * @var string
   */
  public $comparisonOperator;
  protected $comparisonValueType = AlgorithmRulesComparisonValue::class;
  protected $comparisonValueDataType = '';
  protected $signalType = AlgorithmRulesSignal::class;
  protected $signalDataType = '';

  /**
   * @param string
   */
  public function setComparisonOperator($comparisonOperator)
  {
    $this->comparisonOperator = $comparisonOperator;
  }
  /**
   * @return string
   */
  public function getComparisonOperator()
  {
    return $this->comparisonOperator;
  }
  /**
   * @param AlgorithmRulesComparisonValue
   */
  public function setComparisonValue(AlgorithmRulesComparisonValue $comparisonValue)
  {
    $this->comparisonValue = $comparisonValue;
  }
  /**
   * @return AlgorithmRulesComparisonValue
   */
  public function getComparisonValue()
  {
    return $this->comparisonValue;
  }
  /**
   * @param AlgorithmRulesSignal
   */
  public function setSignal(AlgorithmRulesSignal $signal)
  {
    $this->signal = $signal;
  }
  /**
   * @return AlgorithmRulesSignal
   */
  public function getSignal()
  {
    return $this->signal;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AlgorithmRulesSignalComparison::class, 'Google_Service_DisplayVideo_AlgorithmRulesSignalComparison');
