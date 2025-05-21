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

namespace Google\Service\HangoutsChat;

class Space extends \Google\Model
{
  protected $accessSettingsType = AccessSettings::class;
  protected $accessSettingsDataType = '';
  /**
   * @var bool
   */
  public $adminInstalled;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var bool
   */
  public $externalUserAllowed;
  /**
   * @var bool
   */
  public $importMode;
  /**
   * @var string
   */
  public $importModeExpireTime;
  /**
   * @var string
   */
  public $lastActiveTime;
  protected $membershipCountType = MembershipCount::class;
  protected $membershipCountDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $permissionSettingsType = PermissionSettings::class;
  protected $permissionSettingsDataType = '';
  /**
   * @var string
   */
  public $predefinedPermissionSettings;
  /**
   * @var bool
   */
  public $singleUserBotDm;
  protected $spaceDetailsType = SpaceDetails::class;
  protected $spaceDetailsDataType = '';
  /**
   * @var string
   */
  public $spaceHistoryState;
  /**
   * @var string
   */
  public $spaceThreadingState;
  /**
   * @var string
   */
  public $spaceType;
  /**
   * @var string
   */
  public $spaceUri;
  /**
   * @var bool
   */
  public $threaded;
  /**
   * @var string
   */
  public $type;

  /**
   * @param AccessSettings
   */
  public function setAccessSettings(AccessSettings $accessSettings)
  {
    $this->accessSettings = $accessSettings;
  }
  /**
   * @return AccessSettings
   */
  public function getAccessSettings()
  {
    return $this->accessSettings;
  }
  /**
   * @param bool
   */
  public function setAdminInstalled($adminInstalled)
  {
    $this->adminInstalled = $adminInstalled;
  }
  /**
   * @return bool
   */
  public function getAdminInstalled()
  {
    return $this->adminInstalled;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
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
   * @param bool
   */
  public function setExternalUserAllowed($externalUserAllowed)
  {
    $this->externalUserAllowed = $externalUserAllowed;
  }
  /**
   * @return bool
   */
  public function getExternalUserAllowed()
  {
    return $this->externalUserAllowed;
  }
  /**
   * @param bool
   */
  public function setImportMode($importMode)
  {
    $this->importMode = $importMode;
  }
  /**
   * @return bool
   */
  public function getImportMode()
  {
    return $this->importMode;
  }
  /**
   * @param string
   */
  public function setImportModeExpireTime($importModeExpireTime)
  {
    $this->importModeExpireTime = $importModeExpireTime;
  }
  /**
   * @return string
   */
  public function getImportModeExpireTime()
  {
    return $this->importModeExpireTime;
  }
  /**
   * @param string
   */
  public function setLastActiveTime($lastActiveTime)
  {
    $this->lastActiveTime = $lastActiveTime;
  }
  /**
   * @return string
   */
  public function getLastActiveTime()
  {
    return $this->lastActiveTime;
  }
  /**
   * @param MembershipCount
   */
  public function setMembershipCount(MembershipCount $membershipCount)
  {
    $this->membershipCount = $membershipCount;
  }
  /**
   * @return MembershipCount
   */
  public function getMembershipCount()
  {
    return $this->membershipCount;
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
   * @param PermissionSettings
   */
  public function setPermissionSettings(PermissionSettings $permissionSettings)
  {
    $this->permissionSettings = $permissionSettings;
  }
  /**
   * @return PermissionSettings
   */
  public function getPermissionSettings()
  {
    return $this->permissionSettings;
  }
  /**
   * @param string
   */
  public function setPredefinedPermissionSettings($predefinedPermissionSettings)
  {
    $this->predefinedPermissionSettings = $predefinedPermissionSettings;
  }
  /**
   * @return string
   */
  public function getPredefinedPermissionSettings()
  {
    return $this->predefinedPermissionSettings;
  }
  /**
   * @param bool
   */
  public function setSingleUserBotDm($singleUserBotDm)
  {
    $this->singleUserBotDm = $singleUserBotDm;
  }
  /**
   * @return bool
   */
  public function getSingleUserBotDm()
  {
    return $this->singleUserBotDm;
  }
  /**
   * @param SpaceDetails
   */
  public function setSpaceDetails(SpaceDetails $spaceDetails)
  {
    $this->spaceDetails = $spaceDetails;
  }
  /**
   * @return SpaceDetails
   */
  public function getSpaceDetails()
  {
    return $this->spaceDetails;
  }
  /**
   * @param string
   */
  public function setSpaceHistoryState($spaceHistoryState)
  {
    $this->spaceHistoryState = $spaceHistoryState;
  }
  /**
   * @return string
   */
  public function getSpaceHistoryState()
  {
    return $this->spaceHistoryState;
  }
  /**
   * @param string
   */
  public function setSpaceThreadingState($spaceThreadingState)
  {
    $this->spaceThreadingState = $spaceThreadingState;
  }
  /**
   * @return string
   */
  public function getSpaceThreadingState()
  {
    return $this->spaceThreadingState;
  }
  /**
   * @param string
   */
  public function setSpaceType($spaceType)
  {
    $this->spaceType = $spaceType;
  }
  /**
   * @return string
   */
  public function getSpaceType()
  {
    return $this->spaceType;
  }
  /**
   * @param string
   */
  public function setSpaceUri($spaceUri)
  {
    $this->spaceUri = $spaceUri;
  }
  /**
   * @return string
   */
  public function getSpaceUri()
  {
    return $this->spaceUri;
  }
  /**
   * @param bool
   */
  public function setThreaded($threaded)
  {
    $this->threaded = $threaded;
  }
  /**
   * @return bool
   */
  public function getThreaded()
  {
    return $this->threaded;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Space::class, 'Google_Service_HangoutsChat_Space');
