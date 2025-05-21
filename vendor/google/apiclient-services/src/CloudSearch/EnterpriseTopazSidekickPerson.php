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

class EnterpriseTopazSidekickPerson extends \Google\Model
{
  /**
   * @var string
   */
  public $affinityLevel;
  /**
   * @var string
   */
  public $attendingStatus;
  /**
   * @var string
   */
  public $email;
  /**
   * @var string
   */
  public $gaiaId;
  /**
   * @var bool
   */
  public $isGroup;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $obfuscatedGaiaId;
  /**
   * @var string
   */
  public $photoUrl;

  /**
   * @param string
   */
  public function setAffinityLevel($affinityLevel)
  {
    $this->affinityLevel = $affinityLevel;
  }
  /**
   * @return string
   */
  public function getAffinityLevel()
  {
    return $this->affinityLevel;
  }
  /**
   * @param string
   */
  public function setAttendingStatus($attendingStatus)
  {
    $this->attendingStatus = $attendingStatus;
  }
  /**
   * @return string
   */
  public function getAttendingStatus()
  {
    return $this->attendingStatus;
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
   * @param bool
   */
  public function setIsGroup($isGroup)
  {
    $this->isGroup = $isGroup;
  }
  /**
   * @return bool
   */
  public function getIsGroup()
  {
    return $this->isGroup;
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
   * @param string
   */
  public function setObfuscatedGaiaId($obfuscatedGaiaId)
  {
    $this->obfuscatedGaiaId = $obfuscatedGaiaId;
  }
  /**
   * @return string
   */
  public function getObfuscatedGaiaId()
  {
    return $this->obfuscatedGaiaId;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnterpriseTopazSidekickPerson::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickPerson');
