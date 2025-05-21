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

class CheckoutSettings extends \Google\Model
{
  /**
   * @var string
   */
  public $effectiveEnrollmentState;
  /**
   * @var string
   */
  public $effectiveReviewState;
  protected $effectiveUriSettingsType = UrlSettings::class;
  protected $effectiveUriSettingsDataType = '';
  /**
   * @var string
   */
  public $enrollmentState;
  /**
   * @var string
   */
  public $merchantId;
  /**
   * @var string
   */
  public $reviewState;
  protected $uriSettingsType = UrlSettings::class;
  protected $uriSettingsDataType = '';

  /**
   * @param string
   */
  public function setEffectiveEnrollmentState($effectiveEnrollmentState)
  {
    $this->effectiveEnrollmentState = $effectiveEnrollmentState;
  }
  /**
   * @return string
   */
  public function getEffectiveEnrollmentState()
  {
    return $this->effectiveEnrollmentState;
  }
  /**
   * @param string
   */
  public function setEffectiveReviewState($effectiveReviewState)
  {
    $this->effectiveReviewState = $effectiveReviewState;
  }
  /**
   * @return string
   */
  public function getEffectiveReviewState()
  {
    return $this->effectiveReviewState;
  }
  /**
   * @param UrlSettings
   */
  public function setEffectiveUriSettings(UrlSettings $effectiveUriSettings)
  {
    $this->effectiveUriSettings = $effectiveUriSettings;
  }
  /**
   * @return UrlSettings
   */
  public function getEffectiveUriSettings()
  {
    return $this->effectiveUriSettings;
  }
  /**
   * @param string
   */
  public function setEnrollmentState($enrollmentState)
  {
    $this->enrollmentState = $enrollmentState;
  }
  /**
   * @return string
   */
  public function getEnrollmentState()
  {
    return $this->enrollmentState;
  }
  /**
   * @param string
   */
  public function setMerchantId($merchantId)
  {
    $this->merchantId = $merchantId;
  }
  /**
   * @return string
   */
  public function getMerchantId()
  {
    return $this->merchantId;
  }
  /**
   * @param string
   */
  public function setReviewState($reviewState)
  {
    $this->reviewState = $reviewState;
  }
  /**
   * @return string
   */
  public function getReviewState()
  {
    return $this->reviewState;
  }
  /**
   * @param UrlSettings
   */
  public function setUriSettings(UrlSettings $uriSettings)
  {
    $this->uriSettings = $uriSettings;
  }
  /**
   * @return UrlSettings
   */
  public function getUriSettings()
  {
    return $this->uriSettings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CheckoutSettings::class, 'Google_Service_ShoppingContent_CheckoutSettings');
