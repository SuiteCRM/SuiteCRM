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

namespace Google\Service\CloudComposer;

class FetchDatabasePropertiesResponse extends \Google\Model
{
  /**
   * @var bool
   */
  public $isFailoverReplicaAvailable;
  /**
   * @var string
   */
  public $primaryGceZone;
  /**
   * @var string
   */
  public $secondaryGceZone;

  /**
   * @param bool
   */
  public function setIsFailoverReplicaAvailable($isFailoverReplicaAvailable)
  {
    $this->isFailoverReplicaAvailable = $isFailoverReplicaAvailable;
  }
  /**
   * @return bool
   */
  public function getIsFailoverReplicaAvailable()
  {
    return $this->isFailoverReplicaAvailable;
  }
  /**
   * @param string
   */
  public function setPrimaryGceZone($primaryGceZone)
  {
    $this->primaryGceZone = $primaryGceZone;
  }
  /**
   * @return string
   */
  public function getPrimaryGceZone()
  {
    return $this->primaryGceZone;
  }
  /**
   * @param string
   */
  public function setSecondaryGceZone($secondaryGceZone)
  {
    $this->secondaryGceZone = $secondaryGceZone;
  }
  /**
   * @return string
   */
  public function getSecondaryGceZone()
  {
    return $this->secondaryGceZone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FetchDatabasePropertiesResponse::class, 'Google_Service_CloudComposer_FetchDatabasePropertiesResponse');
