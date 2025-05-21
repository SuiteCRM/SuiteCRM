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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1DataQualityRuleStatisticRangeExpectation extends \Google\Model
{
  /**
   * @var string
   */
  public $maxValue;
  /**
   * @var string
   */
  public $minValue;
  /**
   * @var string
   */
  public $statistic;
  /**
   * @var bool
   */
  public $strictMaxEnabled;
  /**
   * @var bool
   */
  public $strictMinEnabled;

  /**
   * @param string
   */
  public function setMaxValue($maxValue)
  {
    $this->maxValue = $maxValue;
  }
  /**
   * @return string
   */
  public function getMaxValue()
  {
    return $this->maxValue;
  }
  /**
   * @param string
   */
  public function setMinValue($minValue)
  {
    $this->minValue = $minValue;
  }
  /**
   * @return string
   */
  public function getMinValue()
  {
    return $this->minValue;
  }
  /**
   * @param string
   */
  public function setStatistic($statistic)
  {
    $this->statistic = $statistic;
  }
  /**
   * @return string
   */
  public function getStatistic()
  {
    return $this->statistic;
  }
  /**
   * @param bool
   */
  public function setStrictMaxEnabled($strictMaxEnabled)
  {
    $this->strictMaxEnabled = $strictMaxEnabled;
  }
  /**
   * @return bool
   */
  public function getStrictMaxEnabled()
  {
    return $this->strictMaxEnabled;
  }
  /**
   * @param bool
   */
  public function setStrictMinEnabled($strictMinEnabled)
  {
    $this->strictMinEnabled = $strictMinEnabled;
  }
  /**
   * @return bool
   */
  public function getStrictMinEnabled()
  {
    return $this->strictMinEnabled;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataQualityRuleStatisticRangeExpectation::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataQualityRuleStatisticRangeExpectation');
