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

namespace Google\Service\SQLAdmin;

class PscConfig extends \Google\Collection
{
  protected $collection_key = 'pscAutoConnections';
  /**
   * @var string[]
   */
  public $allowedConsumerProjects;
  protected $pscAutoConnectionsType = PscAutoConnectionConfig::class;
  protected $pscAutoConnectionsDataType = 'array';
  /**
   * @var bool
   */
  public $pscEnabled;

  /**
   * @param string[]
   */
  public function setAllowedConsumerProjects($allowedConsumerProjects)
  {
    $this->allowedConsumerProjects = $allowedConsumerProjects;
  }
  /**
   * @return string[]
   */
  public function getAllowedConsumerProjects()
  {
    return $this->allowedConsumerProjects;
  }
  /**
   * @param PscAutoConnectionConfig[]
   */
  public function setPscAutoConnections($pscAutoConnections)
  {
    $this->pscAutoConnections = $pscAutoConnections;
  }
  /**
   * @return PscAutoConnectionConfig[]
   */
  public function getPscAutoConnections()
  {
    return $this->pscAutoConnections;
  }
  /**
   * @param bool
   */
  public function setPscEnabled($pscEnabled)
  {
    $this->pscEnabled = $pscEnabled;
  }
  /**
   * @return bool
   */
  public function getPscEnabled()
  {
    return $this->pscEnabled;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PscConfig::class, 'Google_Service_SQLAdmin_PscConfig');
