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

namespace Google\Service\Walletobjects;

class Issuer extends \Google\Model
{
  protected $callbackOptionsType = CallbackOptions::class;
  protected $callbackOptionsDataType = '';
  protected $contactInfoType = IssuerContactInfo::class;
  protected $contactInfoDataType = '';
  /**
   * @var string
   */
  public $homepageUrl;
  /**
   * @var string
   */
  public $issuerId;
  /**
   * @var string
   */
  public $name;
  protected $smartTapMerchantDataType = SmartTapMerchantData::class;
  protected $smartTapMerchantDataDataType = '';

  /**
   * @param CallbackOptions
   */
  public function setCallbackOptions(CallbackOptions $callbackOptions)
  {
    $this->callbackOptions = $callbackOptions;
  }
  /**
   * @return CallbackOptions
   */
  public function getCallbackOptions()
  {
    return $this->callbackOptions;
  }
  /**
   * @param IssuerContactInfo
   */
  public function setContactInfo(IssuerContactInfo $contactInfo)
  {
    $this->contactInfo = $contactInfo;
  }
  /**
   * @return IssuerContactInfo
   */
  public function getContactInfo()
  {
    return $this->contactInfo;
  }
  /**
   * @param string
   */
  public function setHomepageUrl($homepageUrl)
  {
    $this->homepageUrl = $homepageUrl;
  }
  /**
   * @return string
   */
  public function getHomepageUrl()
  {
    return $this->homepageUrl;
  }
  /**
   * @param string
   */
  public function setIssuerId($issuerId)
  {
    $this->issuerId = $issuerId;
  }
  /**
   * @return string
   */
  public function getIssuerId()
  {
    return $this->issuerId;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param SmartTapMerchantData
   */
  public function setSmartTapMerchantData(SmartTapMerchantData $smartTapMerchantData)
  {
    $this->smartTapMerchantData = $smartTapMerchantData;
  }
  /**
   * @return SmartTapMerchantData
   */
  public function getSmartTapMerchantData()
  {
    return $this->smartTapMerchantData;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Issuer::class, 'Google_Service_Walletobjects_Issuer');
