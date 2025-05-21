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

namespace Google\Service\Datastream;

class SalesforceSourceConfig extends \Google\Model
{
  protected $excludeObjectsType = SalesforceOrg::class;
  protected $excludeObjectsDataType = '';
  protected $includeObjectsType = SalesforceOrg::class;
  protected $includeObjectsDataType = '';
  /**
   * @var string
   */
  public $pollingInterval;

  /**
   * @param SalesforceOrg
   */
  public function setExcludeObjects(SalesforceOrg $excludeObjects)
  {
    $this->excludeObjects = $excludeObjects;
  }
  /**
   * @return SalesforceOrg
   */
  public function getExcludeObjects()
  {
    return $this->excludeObjects;
  }
  /**
   * @param SalesforceOrg
   */
  public function setIncludeObjects(SalesforceOrg $includeObjects)
  {
    $this->includeObjects = $includeObjects;
  }
  /**
   * @return SalesforceOrg
   */
  public function getIncludeObjects()
  {
    return $this->includeObjects;
  }
  /**
   * @param string
   */
  public function setPollingInterval($pollingInterval)
  {
    $this->pollingInterval = $pollingInterval;
  }
  /**
   * @return string
   */
  public function getPollingInterval()
  {
    return $this->pollingInterval;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SalesforceSourceConfig::class, 'Google_Service_Datastream_SalesforceSourceConfig');
