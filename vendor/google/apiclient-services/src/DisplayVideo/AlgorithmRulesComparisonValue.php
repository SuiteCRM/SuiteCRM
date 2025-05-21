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

class AlgorithmRulesComparisonValue extends \Google\Model
{
  /**
   * @var bool
   */
  public $boolValue;
  protected $creativeDimensionValueType = Dimensions::class;
  protected $creativeDimensionValueDataType = '';
  protected $dayAndTimeValueType = DayAndTime::class;
  protected $dayAndTimeValueDataType = '';
  /**
   * @var string
   */
  public $deviceTypeValue;
  public $doubleValue;
  /**
   * @var string
   */
  public $environmentValue;
  /**
   * @var string
   */
  public $exchangeValue;
  /**
   * @var string
   */
  public $int64Value;
  /**
   * @var string
   */
  public $onScreenPositionValue;
  /**
   * @var string
   */
  public $stringValue;

  /**
   * @param bool
   */
  public function setBoolValue($boolValue)
  {
    $this->boolValue = $boolValue;
  }
  /**
   * @return bool
   */
  public function getBoolValue()
  {
    return $this->boolValue;
  }
  /**
   * @param Dimensions
   */
  public function setCreativeDimensionValue(Dimensions $creativeDimensionValue)
  {
    $this->creativeDimensionValue = $creativeDimensionValue;
  }
  /**
   * @return Dimensions
   */
  public function getCreativeDimensionValue()
  {
    return $this->creativeDimensionValue;
  }
  /**
   * @param DayAndTime
   */
  public function setDayAndTimeValue(DayAndTime $dayAndTimeValue)
  {
    $this->dayAndTimeValue = $dayAndTimeValue;
  }
  /**
   * @return DayAndTime
   */
  public function getDayAndTimeValue()
  {
    return $this->dayAndTimeValue;
  }
  /**
   * @param string
   */
  public function setDeviceTypeValue($deviceTypeValue)
  {
    $this->deviceTypeValue = $deviceTypeValue;
  }
  /**
   * @return string
   */
  public function getDeviceTypeValue()
  {
    return $this->deviceTypeValue;
  }
  public function setDoubleValue($doubleValue)
  {
    $this->doubleValue = $doubleValue;
  }
  public function getDoubleValue()
  {
    return $this->doubleValue;
  }
  /**
   * @param string
   */
  public function setEnvironmentValue($environmentValue)
  {
    $this->environmentValue = $environmentValue;
  }
  /**
   * @return string
   */
  public function getEnvironmentValue()
  {
    return $this->environmentValue;
  }
  /**
   * @param string
   */
  public function setExchangeValue($exchangeValue)
  {
    $this->exchangeValue = $exchangeValue;
  }
  /**
   * @return string
   */
  public function getExchangeValue()
  {
    return $this->exchangeValue;
  }
  /**
   * @param string
   */
  public function setInt64Value($int64Value)
  {
    $this->int64Value = $int64Value;
  }
  /**
   * @return string
   */
  public function getInt64Value()
  {
    return $this->int64Value;
  }
  /**
   * @param string
   */
  public function setOnScreenPositionValue($onScreenPositionValue)
  {
    $this->onScreenPositionValue = $onScreenPositionValue;
  }
  /**
   * @return string
   */
  public function getOnScreenPositionValue()
  {
    return $this->onScreenPositionValue;
  }
  /**
   * @param string
   */
  public function setStringValue($stringValue)
  {
    $this->stringValue = $stringValue;
  }
  /**
   * @return string
   */
  public function getStringValue()
  {
    return $this->stringValue;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AlgorithmRulesComparisonValue::class, 'Google_Service_DisplayVideo_AlgorithmRulesComparisonValue');
