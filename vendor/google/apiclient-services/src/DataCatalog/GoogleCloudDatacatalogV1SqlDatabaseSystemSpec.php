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

namespace Google\Service\DataCatalog;

class GoogleCloudDatacatalogV1SqlDatabaseSystemSpec extends \Google\Model
{
  /**
   * @var string
   */
  public $databaseVersion;
  /**
   * @var string
   */
  public $instanceHost;
  /**
   * @var string
   */
  public $sqlEngine;

  /**
   * @param string
   */
  public function setDatabaseVersion($databaseVersion)
  {
    $this->databaseVersion = $databaseVersion;
  }
  /**
   * @return string
   */
  public function getDatabaseVersion()
  {
    return $this->databaseVersion;
  }
  /**
   * @param string
   */
  public function setInstanceHost($instanceHost)
  {
    $this->instanceHost = $instanceHost;
  }
  /**
   * @return string
   */
  public function getInstanceHost()
  {
    return $this->instanceHost;
  }
  /**
   * @param string
   */
  public function setSqlEngine($sqlEngine)
  {
    $this->sqlEngine = $sqlEngine;
  }
  /**
   * @return string
   */
  public function getSqlEngine()
  {
    return $this->sqlEngine;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDatacatalogV1SqlDatabaseSystemSpec::class, 'Google_Service_DataCatalog_GoogleCloudDatacatalogV1SqlDatabaseSystemSpec');
