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

class GoogleAdsSearchads360V0ResourcesConversionTrackingSetting extends \Google\Model
{
  /**
   * @var bool
   */
  public $acceptedCustomerDataTerms;
  /**
   * @var string
   */
  public $conversionTrackingId;
  /**
   * @var string
   */
  public $conversionTrackingStatus;
  /**
   * @var string
   */
  public $crossAccountConversionTrackingId;
  /**
   * @var bool
   */
  public $enhancedConversionsForLeadsEnabled;
  /**
   * @var string
   */
  public $googleAdsConversionCustomer;
  /**
   * @var string
   */
  public $googleAdsCrossAccountConversionTrackingId;

  /**
   * @param bool
   */
  public function setAcceptedCustomerDataTerms($acceptedCustomerDataTerms)
  {
    $this->acceptedCustomerDataTerms = $acceptedCustomerDataTerms;
  }
  /**
   * @return bool
   */
  public function getAcceptedCustomerDataTerms()
  {
    return $this->acceptedCustomerDataTerms;
  }
  /**
   * @param string
   */
  public function setConversionTrackingId($conversionTrackingId)
  {
    $this->conversionTrackingId = $conversionTrackingId;
  }
  /**
   * @return string
   */
  public function getConversionTrackingId()
  {
    return $this->conversionTrackingId;
  }
  /**
   * @param string
   */
  public function setConversionTrackingStatus($conversionTrackingStatus)
  {
    $this->conversionTrackingStatus = $conversionTrackingStatus;
  }
  /**
   * @return string
   */
  public function getConversionTrackingStatus()
  {
    return $this->conversionTrackingStatus;
  }
  /**
   * @param string
   */
  public function setCrossAccountConversionTrackingId($crossAccountConversionTrackingId)
  {
    $this->crossAccountConversionTrackingId = $crossAccountConversionTrackingId;
  }
  /**
   * @return string
   */
  public function getCrossAccountConversionTrackingId()
  {
    return $this->crossAccountConversionTrackingId;
  }
  /**
   * @param bool
   */
  public function setEnhancedConversionsForLeadsEnabled($enhancedConversionsForLeadsEnabled)
  {
    $this->enhancedConversionsForLeadsEnabled = $enhancedConversionsForLeadsEnabled;
  }
  /**
   * @return bool
   */
  public function getEnhancedConversionsForLeadsEnabled()
  {
    return $this->enhancedConversionsForLeadsEnabled;
  }
  /**
   * @param string
   */
  public function setGoogleAdsConversionCustomer($googleAdsConversionCustomer)
  {
    $this->googleAdsConversionCustomer = $googleAdsConversionCustomer;
  }
  /**
   * @return string
   */
  public function getGoogleAdsConversionCustomer()
  {
    return $this->googleAdsConversionCustomer;
  }
  /**
   * @param string
   */
  public function setGoogleAdsCrossAccountConversionTrackingId($googleAdsCrossAccountConversionTrackingId)
  {
    $this->googleAdsCrossAccountConversionTrackingId = $googleAdsCrossAccountConversionTrackingId;
  }
  /**
   * @return string
   */
  public function getGoogleAdsCrossAccountConversionTrackingId()
  {
    return $this->googleAdsCrossAccountConversionTrackingId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesConversionTrackingSetting::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesConversionTrackingSetting');
