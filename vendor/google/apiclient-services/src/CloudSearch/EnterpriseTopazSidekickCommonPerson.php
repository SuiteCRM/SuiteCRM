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

namespace Google\Service\CloudSearch;

class EnterpriseTopazSidekickCommonPerson extends \Google\Model
{
  protected $birthdayType = EnterpriseTopazSidekickCommonPersonBirthday::class;
  protected $birthdayDataType = '';
  /**
   * @var string
   */
  public $cellPhone;
  /**
   * @var string
   */
  public $department;
  /**
   * @var string
   */
  public $deskLocation;
  /**
   * @var string
   */
  public $deskPhone;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $email;
  /**
   * @var string
   */
  public $familyName;
  /**
   * @var string
   */
  public $fullAddress;
  /**
   * @var string
   */
  public $gaiaId;
  /**
   * @var string
   */
  public $givenName;
  /**
   * @var string
   */
  public $jobTitle;
  protected $managerType = EnterpriseTopazSidekickCommonPerson::class;
  protected $managerDataType = '';
  /**
   * @var string
   */
  public $obfuscatedId;
  /**
   * @var string
   */
  public $photoUrl;
  /**
   * @var string
   */
  public $streetAddress;

  /**
   * @param EnterpriseTopazSidekickCommonPersonBirthday
   */
  public function setBirthday(EnterpriseTopazSidekickCommonPersonBirthday $birthday)
  {
    $this->birthday = $birthday;
  }
  /**
   * @return EnterpriseTopazSidekickCommonPersonBirthday
   */
  public function getBirthday()
  {
    return $this->birthday;
  }
  /**
   * @param string
   */
  public function setCellPhone($cellPhone)
  {
    $this->cellPhone = $cellPhone;
  }
  /**
   * @return string
   */
  public function getCellPhone()
  {
    return $this->cellPhone;
  }
  /**
   * @param string
   */
  public function setDepartment($department)
  {
    $this->department = $department;
  }
  /**
   * @return string
   */
  public function getDepartment()
  {
    return $this->department;
  }
  /**
   * @param string
   */
  public function setDeskLocation($deskLocation)
  {
    $this->deskLocation = $deskLocation;
  }
  /**
   * @return string
   */
  public function getDeskLocation()
  {
    return $this->deskLocation;
  }
  /**
   * @param string
   */
  public function setDeskPhone($deskPhone)
  {
    $this->deskPhone = $deskPhone;
  }
  /**
   * @return string
   */
  public function getDeskPhone()
  {
    return $this->deskPhone;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setEmail($email)
  {
    $this->email = $email;
  }
  /**
   * @return string
   */
  public function getEmail()
  {
    return $this->email;
  }
  /**
   * @param string
   */
  public function setFamilyName($familyName)
  {
    $this->familyName = $familyName;
  }
  /**
   * @return string
   */
  public function getFamilyName()
  {
    return $this->familyName;
  }
  /**
   * @param string
   */
  public function setFullAddress($fullAddress)
  {
    $this->fullAddress = $fullAddress;
  }
  /**
   * @return string
   */
  public function getFullAddress()
  {
    return $this->fullAddress;
  }
  /**
   * @param string
   */
  public function setGaiaId($gaiaId)
  {
    $this->gaiaId = $gaiaId;
  }
  /**
   * @return string
   */
  public function getGaiaId()
  {
    return $this->gaiaId;
  }
  /**
   * @param string
   */
  public function setGivenName($givenName)
  {
    $this->givenName = $givenName;
  }
  /**
   * @return string
   */
  public function getGivenName()
  {
    return $this->givenName;
  }
  /**
   * @param string
   */
  public function setJobTitle($jobTitle)
  {
    $this->jobTitle = $jobTitle;
  }
  /**
   * @return string
   */
  public function getJobTitle()
  {
    return $this->jobTitle;
  }
  /**
   * @param EnterpriseTopazSidekickCommonPerson
   */
  public function setManager(EnterpriseTopazSidekickCommonPerson $manager)
  {
    $this->manager = $manager;
  }
  /**
   * @return EnterpriseTopazSidekickCommonPerson
   */
  public function getManager()
  {
    return $this->manager;
  }
  /**
   * @param string
   */
  public function setObfuscatedId($obfuscatedId)
  {
    $this->obfuscatedId = $obfuscatedId;
  }
  /**
   * @return string
   */
  public function getObfuscatedId()
  {
    return $this->obfuscatedId;
  }
  /**
   * @param string
   */
  public function setPhotoUrl($photoUrl)
  {
    $this->photoUrl = $photoUrl;
  }
  /**
   * @return string
   */
  public function getPhotoUrl()
  {
    return $this->photoUrl;
  }
  /**
   * @param string
   */
  public function setStreetAddress($streetAddress)
  {
    $this->streetAddress = $streetAddress;
  }
  /**
   * @return string
   */
  public function getStreetAddress()
  {
    return $this->streetAddress;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnterpriseTopazSidekickCommonPerson::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickCommonPerson');
