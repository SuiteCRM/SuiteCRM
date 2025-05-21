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

class ConversionSource extends \Google\Model
{
  /**
   * @var string
   */
  public $conversionSourceId;
  /**
   * @var string
   */
  public $expireTime;
  protected $googleAnalyticsLinkType = GoogleAnalyticsLink::class;
  protected $googleAnalyticsLinkDataType = '';
  protected $merchantCenterDestinationType = MerchantCenterDestination::class;
  protected $merchantCenterDestinationDataType = '';
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setConversionSourceId($conversionSourceId)
  {
    $this->conversionSourceId = $conversionSourceId;
  }
  /**
   * @return string
   */
  public function getConversionSourceId()
  {
    return $this->conversionSourceId;
  }
  /**
   * @param string
   */
  public function setExpireTime($expireTime)
  {
    $this->expireTime = $expireTime;
  }
  /**
   * @return string
   */
  public function getExpireTime()
  {
    return $this->expireTime;
  }
  /**
   * @param GoogleAnalyticsLink
   */
  public function setGoogleAnalyticsLink(GoogleAnalyticsLink $googleAnalyticsLink)
  {
    $this->googleAnalyticsLink = $googleAnalyticsLink;
  }
  /**
   * @return GoogleAnalyticsLink
   */
  public function getGoogleAnalyticsLink()
  {
    return $this->googleAnalyticsLink;
  }
  /**
   * @param MerchantCenterDestination
   */
  public function setMerchantCenterDestination(MerchantCenterDestination $merchantCenterDestination)
  {
    $this->merchantCenterDestination = $merchantCenterDestination;
  }
  /**
   * @return MerchantCenterDestination
   */
  public function getMerchantCenterDestination()
  {
    return $this->merchantCenterDestination;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ConversionSource::class, 'Google_Service_ShoppingContent_ConversionSource');
