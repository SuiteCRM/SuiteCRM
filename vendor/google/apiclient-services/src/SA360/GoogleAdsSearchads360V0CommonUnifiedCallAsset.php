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

class GoogleAdsSearchads360V0CommonUnifiedCallAsset extends \Google\Collection
{
  protected $collection_key = 'adScheduleTargets';
  protected $adScheduleTargetsType = GoogleAdsSearchads360V0CommonAdScheduleInfo::class;
  protected $adScheduleTargetsDataType = 'array';
  /**
   * @var string
   */
  public $callConversionAction;
  /**
   * @var string
   */
  public $callConversionReportingState;
  /**
   * @var bool
   */
  public $callOnly;
  /**
   * @var bool
   */
  public $callTrackingEnabled;
  /**
   * @var string
   */
  public $countryCode;
  /**
   * @var string
   */
  public $endDate;
  /**
   * @var string
   */
  public $phoneNumber;
  /**
   * @var string
   */
  public $startDate;
  /**
   * @var bool
   */
  public $useSearcherTimeZone;

  /**
   * @param GoogleAdsSearchads360V0CommonAdScheduleInfo[]
   */
  public function setAdScheduleTargets($adScheduleTargets)
  {
    $this->adScheduleTargets = $adScheduleTargets;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonAdScheduleInfo[]
   */
  public function getAdScheduleTargets()
  {
    return $this->adScheduleTargets;
  }
  /**
   * @param string
   */
  public function setCallConversionAction($callConversionAction)
  {
    $this->callConversionAction = $callConversionAction;
  }
  /**
   * @return string
   */
  public function getCallConversionAction()
  {
    return $this->callConversionAction;
  }
  /**
   * @param string
   */
  public function setCallConversionReportingState($callConversionReportingState)
  {
    $this->callConversionReportingState = $callConversionReportingState;
  }
  /**
   * @return string
   */
  public function getCallConversionReportingState()
  {
    return $this->callConversionReportingState;
  }
  /**
   * @param bool
   */
  public function setCallOnly($callOnly)
  {
    $this->callOnly = $callOnly;
  }
  /**
   * @return bool
   */
  public function getCallOnly()
  {
    return $this->callOnly;
  }
  /**
   * @param bool
   */
  public function setCallTrackingEnabled($callTrackingEnabled)
  {
    $this->callTrackingEnabled = $callTrackingEnabled;
  }
  /**
   * @return bool
   */
  public function getCallTrackingEnabled()
  {
    return $this->callTrackingEnabled;
  }
  /**
   * @param string
   */
  public function setCountryCode($countryCode)
  {
    $this->countryCode = $countryCode;
  }
  /**
   * @return string
   */
  public function getCountryCode()
  {
    return $this->countryCode;
  }
  /**
   * @param string
   */
  public function setEndDate($endDate)
  {
    $this->endDate = $endDate;
  }
  /**
   * @return string
   */
  public function getEndDate()
  {
    return $this->endDate;
  }
  /**
   * @param string
   */
  public function setPhoneNumber($phoneNumber)
  {
    $this->phoneNumber = $phoneNumber;
  }
  /**
   * @return string
   */
  public function getPhoneNumber()
  {
    return $this->phoneNumber;
  }
  /**
   * @param string
   */
  public function setStartDate($startDate)
  {
    $this->startDate = $startDate;
  }
  /**
   * @return string
   */
  public function getStartDate()
  {
    return $this->startDate;
  }
  /**
   * @param bool
   */
  public function setUseSearcherTimeZone($useSearcherTimeZone)
  {
    $this->useSearcherTimeZone = $useSearcherTimeZone;
  }
  /**
   * @return bool
   */
  public function getUseSearcherTimeZone()
  {
    return $this->useSearcherTimeZone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0CommonUnifiedCallAsset::class, 'Google_Service_SA360_GoogleAdsSearchads360V0CommonUnifiedCallAsset');
