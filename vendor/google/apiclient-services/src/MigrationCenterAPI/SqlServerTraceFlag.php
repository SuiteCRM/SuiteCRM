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

namespace Google\Service\MigrationCenterAPI;

class SqlServerTraceFlag extends \Google\Model
{
  /**
   * @var string
   */
  public $scope;
  /**
   * @var string
   */
  public $traceFlagName;

  /**
   * @param string
   */
  public function setScope($scope)
  {
    $this->scope = $scope;
  }
  /**
   * @return string
   */
  public function getScope()
  {
    return $this->scope;
  }
  /**
   * @param string
   */
  public function setTraceFlagName($traceFlagName)
  {
    $this->traceFlagName = $traceFlagName;
  }
  /**
   * @return string
   */
  public function getTraceFlagName()
  {
    return $this->traceFlagName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SqlServerTraceFlag::class, 'Google_Service_MigrationCenterAPI_SqlServerTraceFlag');
