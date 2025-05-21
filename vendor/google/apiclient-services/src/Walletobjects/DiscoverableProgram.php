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

class DiscoverableProgram extends \Google\Model
{
  protected $merchantSigninInfoType = DiscoverableProgramMerchantSigninInfo::class;
  protected $merchantSigninInfoDataType = '';
  protected $merchantSignupInfoType = DiscoverableProgramMerchantSignupInfo::class;
  protected $merchantSignupInfoDataType = '';
  /**
   * @var string
   */
  public $state;

  /**
   * @param DiscoverableProgramMerchantSigninInfo
   */
  public function setMerchantSigninInfo(DiscoverableProgramMerchantSigninInfo $merchantSigninInfo)
  {
    $this->merchantSigninInfo = $merchantSigninInfo;
  }
  /**
   * @return DiscoverableProgramMerchantSigninInfo
   */
  public function getMerchantSigninInfo()
  {
    return $this->merchantSigninInfo;
  }
  /**
   * @param DiscoverableProgramMerchantSignupInfo
   */
  public function setMerchantSignupInfo(DiscoverableProgramMerchantSignupInfo $merchantSignupInfo)
  {
    $this->merchantSignupInfo = $merchantSignupInfo;
  }
  /**
   * @return DiscoverableProgramMerchantSignupInfo
   */
  public function getMerchantSignupInfo()
  {
    return $this->merchantSignupInfo;
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
class_alias(DiscoverableProgram::class, 'Google_Service_Walletobjects_DiscoverableProgram');
