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

namespace Google\Service\RecaptchaEnterprise;

class GoogleCloudRecaptchaenterpriseV1TransactionDataGatewayInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $avsResponseCode;
  /**
   * @var string
   */
  public $cvvResponseCode;
  /**
   * @var string
   */
  public $gatewayResponseCode;
  /**
   * @var string
   */
  public $name;

  /**
   * @param string
   */
  public function setAvsResponseCode($avsResponseCode)
  {
    $this->avsResponseCode = $avsResponseCode;
  }
  /**
   * @return string
   */
  public function getAvsResponseCode()
  {
    return $this->avsResponseCode;
  }
  /**
   * @param string
   */
  public function setCvvResponseCode($cvvResponseCode)
  {
    $this->cvvResponseCode = $cvvResponseCode;
  }
  /**
   * @return string
   */
  public function getCvvResponseCode()
  {
    return $this->cvvResponseCode;
  }
  /**
   * @param string
   */
  public function setGatewayResponseCode($gatewayResponseCode)
  {
    $this->gatewayResponseCode = $gatewayResponseCode;
  }
  /**
   * @return string
   */
  public function getGatewayResponseCode()
  {
    return $this->gatewayResponseCode;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRecaptchaenterpriseV1TransactionDataGatewayInfo::class, 'Google_Service_RecaptchaEnterprise_GoogleCloudRecaptchaenterpriseV1TransactionDataGatewayInfo');
