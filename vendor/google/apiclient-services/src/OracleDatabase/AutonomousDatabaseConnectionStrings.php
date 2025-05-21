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

namespace Google\Service\OracleDatabase;

class AutonomousDatabaseConnectionStrings extends \Google\Collection
{
  protected $collection_key = 'profiles';
  protected $allConnectionStringsType = AllConnectionStrings::class;
  protected $allConnectionStringsDataType = '';
  /**
   * @var string
   */
  public $dedicated;
  /**
   * @var string
   */
  public $high;
  /**
   * @var string
   */
  public $low;
  /**
   * @var string
   */
  public $medium;
  protected $profilesType = DatabaseConnectionStringProfile::class;
  protected $profilesDataType = 'array';

  /**
   * @param AllConnectionStrings
   */
  public function setAllConnectionStrings(AllConnectionStrings $allConnectionStrings)
  {
    $this->allConnectionStrings = $allConnectionStrings;
  }
  /**
   * @return AllConnectionStrings
   */
  public function getAllConnectionStrings()
  {
    return $this->allConnectionStrings;
  }
  /**
   * @param string
   */
  public function setDedicated($dedicated)
  {
    $this->dedicated = $dedicated;
  }
  /**
   * @return string
   */
  public function getDedicated()
  {
    return $this->dedicated;
  }
  /**
   * @param string
   */
  public function setHigh($high)
  {
    $this->high = $high;
  }
  /**
   * @return string
   */
  public function getHigh()
  {
    return $this->high;
  }
  /**
   * @param string
   */
  public function setLow($low)
  {
    $this->low = $low;
  }
  /**
   * @return string
   */
  public function getLow()
  {
    return $this->low;
  }
  /**
   * @param string
   */
  public function setMedium($medium)
  {
    $this->medium = $medium;
  }
  /**
   * @return string
   */
  public function getMedium()
  {
    return $this->medium;
  }
  /**
   * @param DatabaseConnectionStringProfile[]
   */
  public function setProfiles($profiles)
  {
    $this->profiles = $profiles;
  }
  /**
   * @return DatabaseConnectionStringProfile[]
   */
  public function getProfiles()
  {
    return $this->profiles;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutonomousDatabaseConnectionStrings::class, 'Google_Service_OracleDatabase_AutonomousDatabaseConnectionStrings');
