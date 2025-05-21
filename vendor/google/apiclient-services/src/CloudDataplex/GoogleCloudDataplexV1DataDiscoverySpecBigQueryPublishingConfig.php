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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1DataDiscoverySpecBigQueryPublishingConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $connection;
  /**
   * @var string
   */
  public $tableType;

  /**
   * @param string
   */
  public function setConnection($connection)
  {
    $this->connection = $connection;
  }
  /**
   * @return string
   */
  public function getConnection()
  {
    return $this->connection;
  }
  /**
   * @param string
   */
  public function setTableType($tableType)
  {
    $this->tableType = $tableType;
  }
  /**
   * @return string
   */
  public function getTableType()
  {
    return $this->tableType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataDiscoverySpecBigQueryPublishingConfig::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataDiscoverySpecBigQueryPublishingConfig');
