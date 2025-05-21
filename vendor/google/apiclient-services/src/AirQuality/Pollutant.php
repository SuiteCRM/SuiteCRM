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

namespace Google\Service\AirQuality;

class Pollutant extends \Google\Model
{
  protected $additionalInfoType = AdditionalInfo::class;
  protected $additionalInfoDataType = '';
  /**
   * @var string
   */
  public $code;
  protected $concentrationType = Concentration::class;
  protected $concentrationDataType = '';
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $fullName;

  /**
   * @param AdditionalInfo
   */
  public function setAdditionalInfo(AdditionalInfo $additionalInfo)
  {
    $this->additionalInfo = $additionalInfo;
  }
  /**
   * @return AdditionalInfo
   */
  public function getAdditionalInfo()
  {
    return $this->additionalInfo;
  }
  /**
   * @param string
   */
  public function setCode($code)
  {
    $this->code = $code;
  }
  /**
   * @return string
   */
  public function getCode()
  {
    return $this->code;
  }
  /**
   * @param Concentration
   */
  public function setConcentration(Concentration $concentration)
  {
    $this->concentration = $concentration;
  }
  /**
   * @return Concentration
   */
  public function getConcentration()
  {
    return $this->concentration;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setFullName($fullName)
  {
    $this->fullName = $fullName;
  }
  /**
   * @return string
   */
  public function getFullName()
  {
    return $this->fullName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Pollutant::class, 'Google_Service_AirQuality_Pollutant');
