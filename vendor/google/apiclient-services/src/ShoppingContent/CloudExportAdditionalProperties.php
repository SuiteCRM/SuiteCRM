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

namespace Google\Service\ShoppingContent;

class CloudExportAdditionalProperties extends \Google\Collection
{
  protected $collection_key = 'textValue';
  /**
   * @var bool
   */
  public $boolValue;
  /**
   * @var float[]
   */
  public $floatValue;
  /**
   * @var string[]
   */
  public $intValue;
  /**
   * @var float
   */
  public $maxValue;
  /**
   * @var float
   */
  public $minValue;
  /**
   * @var string
   */
  public $propertyName;
  /**
   * @var string[]
   */
  public $textValue;
  /**
   * @var string
   */
  public $unitCode;

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
   * @param float[]
   */
  public function setFloatValue($floatValue)
  {
    $this->floatValue = $floatValue;
  }
  /**
   * @return float[]
   */
  public function getFloatValue()
  {
    return $this->floatValue;
  }
  /**
   * @param string[]
   */
  public function setIntValue($intValue)
  {
    $this->intValue = $intValue;
  }
  /**
   * @return string[]
   */
  public function getIntValue()
  {
    return $this->intValue;
  }
  /**
   * @param float
   */
  public function setMaxValue($maxValue)
  {
    $this->maxValue = $maxValue;
  }
  /**
   * @return float
   */
  public function getMaxValue()
  {
    return $this->maxValue;
  }
  /**
   * @param float
   */
  public function setMinValue($minValue)
  {
    $this->minValue = $minValue;
  }
  /**
   * @return float
   */
  public function getMinValue()
  {
    return $this->minValue;
  }
  /**
   * @param string
   */
  public function setPropertyName($propertyName)
  {
    $this->propertyName = $propertyName;
  }
  /**
   * @return string
   */
  public function getPropertyName()
  {
    return $this->propertyName;
  }
  /**
   * @param string[]
   */
  public function setTextValue($textValue)
  {
    $this->textValue = $textValue;
  }
  /**
   * @return string[]
   */
  public function getTextValue()
  {
    return $this->textValue;
  }
  /**
   * @param string
   */
  public function setUnitCode($unitCode)
  {
    $this->unitCode = $unitCode;
  }
  /**
   * @return string
   */
  public function getUnitCode()
  {
    return $this->unitCode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudExportAdditionalProperties::class, 'Google_Service_ShoppingContent_CloudExportAdditionalProperties');
