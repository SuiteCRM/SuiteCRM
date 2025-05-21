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

namespace Google\Service\Dfareporting;

class OfflineUserAddressInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $city;
  /**
   * @var string
   */
  public $countryCode;
  /**
   * @var string
   */
  public $hashedFirstName;
  /**
   * @var string
   */
  public $hashedLastName;
  /**
   * @var string
   */
  public $hashedStreetAddress;
  /**
   * @var string
   */
  public $postalCode;
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setCity($city)
  {
    $this->city = $city;
  }
  /**
   * @return string
   */
  public function getCity()
  {
    return $this->city;
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
  public function setHashedFirstName($hashedFirstName)
  {
    $this->hashedFirstName = $hashedFirstName;
  }
  /**
   * @return string
   */
  public function getHashedFirstName()
  {
    return $this->hashedFirstName;
  }
  /**
   * @param string
   */
  public function setHashedLastName($hashedLastName)
  {
    $this->hashedLastName = $hashedLastName;
  }
  /**
   * @return string
   */
  public function getHashedLastName()
  {
    return $this->hashedLastName;
  }
  /**
   * @param string
   */
  public function setHashedStreetAddress($hashedStreetAddress)
  {
    $this->hashedStreetAddress = $hashedStreetAddress;
  }
  /**
   * @return string
   */
  public function getHashedStreetAddress()
  {
    return $this->hashedStreetAddress;
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
class_alias(OfflineUserAddressInfo::class, 'Google_Service_Dfareporting_OfflineUserAddressInfo');
