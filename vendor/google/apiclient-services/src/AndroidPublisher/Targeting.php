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

namespace Google\Service\AndroidPublisher;

class Targeting extends \Google\Model
{
  protected $allUsersType = AllUsers::class;
  protected $allUsersDataType = '';
  protected $androidSdksType = AndroidSdks::class;
  protected $androidSdksDataType = '';
  protected $regionsType = Regions::class;
  protected $regionsDataType = '';
  protected $versionListType = AppVersionList::class;
  protected $versionListDataType = '';
  protected $versionRangeType = AppVersionRange::class;
  protected $versionRangeDataType = '';

  /**
   * @param AllUsers
   */
  public function setAllUsers(AllUsers $allUsers)
  {
    $this->allUsers = $allUsers;
  }
  /**
   * @return AllUsers
   */
  public function getAllUsers()
  {
    return $this->allUsers;
  }
  /**
   * @param AndroidSdks
   */
  public function setAndroidSdks(AndroidSdks $androidSdks)
  {
    $this->androidSdks = $androidSdks;
  }
  /**
   * @return AndroidSdks
   */
  public function getAndroidSdks()
  {
    return $this->androidSdks;
  }
  /**
   * @param Regions
   */
  public function setRegions(Regions $regions)
  {
    $this->regions = $regions;
  }
  /**
   * @return Regions
   */
  public function getRegions()
  {
    return $this->regions;
  }
  /**
   * @param AppVersionList
   */
  public function setVersionList(AppVersionList $versionList)
  {
    $this->versionList = $versionList;
  }
  /**
   * @return AppVersionList
   */
  public function getVersionList()
  {
    return $this->versionList;
  }
  /**
   * @param AppVersionRange
   */
  public function setVersionRange(AppVersionRange $versionRange)
  {
    $this->versionRange = $versionRange;
  }
  /**
   * @return AppVersionRange
   */
  public function getVersionRange()
  {
    return $this->versionRange;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Targeting::class, 'Google_Service_AndroidPublisher_Targeting');
