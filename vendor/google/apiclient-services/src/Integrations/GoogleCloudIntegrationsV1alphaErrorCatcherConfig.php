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

namespace Google\Service\Integrations;

class GoogleCloudIntegrationsV1alphaErrorCatcherConfig extends \Google\Collection
{
  protected $collection_key = 'startErrorTasks';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $errorCatcherId;
  /**
   * @var string
   */
  public $errorCatcherNumber;
  /**
   * @var string
   */
  public $label;
  protected $positionType = GoogleCloudIntegrationsV1alphaCoordinate::class;
  protected $positionDataType = '';
  protected $startErrorTasksType = GoogleCloudIntegrationsV1alphaNextTask::class;
  protected $startErrorTasksDataType = 'array';

  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setErrorCatcherId($errorCatcherId)
  {
    $this->errorCatcherId = $errorCatcherId;
  }
  /**
   * @return string
   */
  public function getErrorCatcherId()
  {
    return $this->errorCatcherId;
  }
  /**
   * @param string
   */
  public function setErrorCatcherNumber($errorCatcherNumber)
  {
    $this->errorCatcherNumber = $errorCatcherNumber;
  }
  /**
   * @return string
   */
  public function getErrorCatcherNumber()
  {
    return $this->errorCatcherNumber;
  }
  /**
   * @param string
   */
  public function setLabel($label)
  {
    $this->label = $label;
  }
  /**
   * @return string
   */
  public function getLabel()
  {
    return $this->label;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaCoordinate
   */
  public function setPosition(GoogleCloudIntegrationsV1alphaCoordinate $position)
  {
    $this->position = $position;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaCoordinate
   */
  public function getPosition()
  {
    return $this->position;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaNextTask[]
   */
  public function setStartErrorTasks($startErrorTasks)
  {
    $this->startErrorTasks = $startErrorTasks;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaNextTask[]
   */
  public function getStartErrorTasks()
  {
    return $this->startErrorTasks;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaErrorCatcherConfig::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaErrorCatcherConfig');
