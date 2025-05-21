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

class AutonomousDbVersion extends \Google\Model
{
  /**
   * @var string
   */
  public $dbWorkload;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $version;
  /**
   * @var string
   */
  public $workloadUri;

  /**
   * @param string
   */
  public function setDbWorkload($dbWorkload)
  {
    $this->dbWorkload = $dbWorkload;
  }
  /**
   * @return string
   */
  public function getDbWorkload()
  {
    return $this->dbWorkload;
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
  public function setVersion($version)
  {
    $this->version = $version;
  }
  /**
   * @return string
   */
  public function getVersion()
  {
    return $this->version;
  }
  /**
   * @param string
   */
  public function setWorkloadUri($workloadUri)
  {
    $this->workloadUri = $workloadUri;
  }
  /**
   * @return string
   */
  public function getWorkloadUri()
  {
    return $this->workloadUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutonomousDbVersion::class, 'Google_Service_OracleDatabase_AutonomousDbVersion');
