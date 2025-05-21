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

class GoogleAdsSearchads360V0CommonSearchAds360TextAdInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $adTrackingId;
  /**
   * @var string
   */
  public $description1;
  /**
   * @var string
   */
  public $description2;
  /**
   * @var string
   */
  public $displayMobileUrl;
  /**
   * @var string
   */
  public $displayUrl;
  /**
   * @var string
   */
  public $headline;

  /**
   * @param string
   */
  public function setAdTrackingId($adTrackingId)
  {
    $this->adTrackingId = $adTrackingId;
  }
  /**
   * @return string
   */
  public function getAdTrackingId()
  {
    return $this->adTrackingId;
  }
  /**
   * @param string
   */
  public function setDescription1($description1)
  {
    $this->description1 = $description1;
  }
  /**
   * @return string
   */
  public function getDescription1()
  {
    return $this->description1;
  }
  /**
   * @param string
   */
  public function setDescription2($description2)
  {
    $this->description2 = $description2;
  }
  /**
   * @return string
   */
  public function getDescription2()
  {
    return $this->description2;
  }
  /**
   * @param string
   */
  public function setDisplayMobileUrl($displayMobileUrl)
  {
    $this->displayMobileUrl = $displayMobileUrl;
  }
  /**
   * @return string
   */
  public function getDisplayMobileUrl()
  {
    return $this->displayMobileUrl;
  }
  /**
   * @param string
   */
  public function setDisplayUrl($displayUrl)
  {
    $this->displayUrl = $displayUrl;
  }
  /**
   * @return string
   */
  public function getDisplayUrl()
  {
    return $this->displayUrl;
  }
  /**
   * @param string
   */
  public function setHeadline($headline)
  {
    $this->headline = $headline;
  }
  /**
   * @return string
   */
  public function getHeadline()
  {
    return $this->headline;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0CommonSearchAds360TextAdInfo::class, 'Google_Service_SA360_GoogleAdsSearchads360V0CommonSearchAds360TextAdInfo');
