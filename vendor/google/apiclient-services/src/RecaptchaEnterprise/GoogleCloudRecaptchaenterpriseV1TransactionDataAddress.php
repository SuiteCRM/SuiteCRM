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

class GoogleCloudRecaptchaenterpriseV1TransactionDataAddress extends \Google\Collection
{
  protected $collection_key = 'address';
  /**
   * @var string[]
   */
  public $address;
  /**
   * @var string
   */
  public $administrativeArea;
  /**
   * @var string
   */
  public $locality;
  /**
   * @var string
   */
  public $postalCode;
  /**
   * @var string
   */
  public $recipient;
  /**
   * @var string
   */
  public $regionCode;

  /**
   * @param string[]
   */
  public function setAddress($address)
  {
    $this->address = $address;
  }
  /**
   * @return string[]
   */
  public function getAddress()
  {
    return $this->address;
  }
  /**
   * @param string
   */
  public function setAdministrativeArea($administrativeArea)
  {
    $this->administrativeArea = $administrativeArea;
  }
  /**
   * @return string
   */
  public function getAdministrativeArea()
  {
    return $this->administrativeArea;
  }
  /**
   * @param string
   */
  public function setLocality($locality)
  {
    $this->locality = $locality;
  }
  /**
   * @return string
   */
  public function getLocality()
  {
    return $this->locality;
  }
  /**
   * @param string
   */
  public function setPostalCode($postalCode)
  {
    $this->postalCode = $postalCode;
  }
  /**
   * @return string
   */
  public function getPostalCode()
  {
    return $this->postalCode;
  }
  /**
   * @param string
   */
  public function setRecipient($recipient)
  {
    $this->recipient = $recipient;
  }
  /**
   * @return string
   */
  public function getRecipient()
  {
    return $this->recipient;
  }
  /**
   * @param string
   */
  public function setRegionCode($regionCode)
  {
    $this->regionCode = $regionCode;
  }
  /**
   * @return string
   */
  public function getRegionCode()
  {
    return $this->regionCode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRecaptchaenterpriseV1TransactionDataAddress::class, 'Google_Service_RecaptchaEnterprise_GoogleCloudRecaptchaenterpriseV1TransactionDataAddress');
