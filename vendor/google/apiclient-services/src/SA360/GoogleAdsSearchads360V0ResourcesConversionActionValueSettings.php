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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0ResourcesConversionActionValueSettings extends \Google\Model
{
  /**
   * @var bool
   */
  public $alwaysUseDefaultValue;
  /**
   * @var string
   */
  public $defaultCurrencyCode;
  public $defaultValue;

  /**
   * @param bool
   */
  public function setAlwaysUseDefaultValue($alwaysUseDefaultValue)
  {
    $this->alwaysUseDefaultValue = $alwaysUseDefaultValue;
  }
  /**
   * @return bool
   */
  public function getAlwaysUseDefaultValue()
  {
    return $this->alwaysUseDefaultValue;
  }
  /**
   * @param string
   */
  public function setDefaultCurrencyCode($defaultCurrencyCode)
  {
    $this->defaultCurrencyCode = $defaultCurrencyCode;
  }
  /**
   * @return string
   */
  public function getDefaultCurrencyCode()
  {
    return $this->defaultCurrencyCode;
  }
  public function setDefaultValue($defaultValue)
  {
    $this->defaultValue = $defaultValue;
  }
  public function getDefaultValue()
  {
    return $this->defaultValue;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesConversionActionValueSettings::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesConversionActionValueSettings');
