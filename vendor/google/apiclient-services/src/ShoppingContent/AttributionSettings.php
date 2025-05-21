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

class AttributionSettings extends \Google\Collection
{
  protected $collection_key = 'conversionType';
  /**
   * @var int
   */
  public $attributionLookbackWindowInDays;
  /**
   * @var string
   */
  public $attributionModel;
  protected $conversionTypeType = AttributionSettingsConversionType::class;
  protected $conversionTypeDataType = 'array';

  /**
   * @param int
   */
  public function setAttributionLookbackWindowInDays($attributionLookbackWindowInDays)
  {
    $this->attributionLookbackWindowInDays = $attributionLookbackWindowInDays;
  }
  /**
   * @return int
   */
  public function getAttributionLookbackWindowInDays()
  {
    return $this->attributionLookbackWindowInDays;
  }
  /**
   * @param string
   */
  public function setAttributionModel($attributionModel)
  {
    $this->attributionModel = $attributionModel;
  }
  /**
   * @return string
   */
  public function getAttributionModel()
  {
    return $this->attributionModel;
  }
  /**
   * @param AttributionSettingsConversionType[]
   */
  public function setConversionType($conversionType)
  {
    $this->conversionType = $conversionType;
  }
  /**
   * @return AttributionSettingsConversionType[]
   */
  public function getConversionType()
  {
    return $this->conversionType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AttributionSettings::class, 'Google_Service_ShoppingContent_AttributionSettings');
