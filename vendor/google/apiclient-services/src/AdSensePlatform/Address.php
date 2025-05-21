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

namespace Google\Service\AdSensePlatform;

class Address extends \Google\Model
{
  /**
   * @var string
   */
  public $address1;
  /**
   * @var string
   */
  public $address2;
  /**
   * @var string
   */
  public $city;
  /**
   * @var string
   */
  public $company;
  /**
   * @var string
   */
  public $contact;
  /**
   * @var string
   */
  public $fax;
  /**
   * @var string
   */
  public $phone;
  /**
   * @var string
   */
  public $regionCode;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $zip;

  /**
   * @param string
   */
  public function setAddress1($address1)
  {
    $this->address1 = $address1;
  }
  /**
   * @return string
   */
  public function getAddress1()
  {
    return $this->address1;
  }
  /**
   * @param string
   */
  public function setAddress2($address2)
  {
    $this->address2 = $address2;
  }
  /**
   * @return string
   */
  public function getAddress2()
  {
    return $this->address2;
  }
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
  public function setCompany($company)
  {
    $this->company = $company;
  }
  /**
   * @return string
   */
  public function getCompany()
  {
    return $this->company;
  }
  /**
   * @param string
   */
  public function setContact($contact)
  {
    $this->contact = $contact;
  }
  /**
   * @return string
   */
  public function getContact()
  {
    return $this->contact;
  }
  /**
   * @param string
   */
  public function setFax($fax)
  {
    $this->fax = $fax;
  }
  /**
   * @return string
   */
  public function getFax()
  {
    return $this->fax;
  }
  /**
   * @param string
   */
  public function setPhone($phone)
  {
    $this->phone = $phone;
  }
  /**
   * @return string
   */
  public function getPhone()
  {
    return $this->phone;
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
  /**
   * @param string
   */
  public function setZip($zip)
  {
    $this->zip = $zip;
  }
  /**
   * @return string
   */
  public function getZip()
  {
    return $this->zip;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Address::class, 'Google_Service_AdSensePlatform_Address');
