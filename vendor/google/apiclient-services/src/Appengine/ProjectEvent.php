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

namespace Google\Service\Appengine;

class ProjectEvent extends \Google\Model
{
  /**
   * @var string
   */
  public $eventId;
  /**
   * @var string
   */
  public $phase;
  protected $projectMetadataType = ProjectsMetadata::class;
  protected $projectMetadataDataType = '';
  protected $stateType = ContainerState::class;
  protected $stateDataType = '';

  /**
   * @param string
   */
  public function setEventId($eventId)
  {
    $this->eventId = $eventId;
  }
  /**
   * @return string
   */
  public function getEventId()
  {
    return $this->eventId;
  }
  /**
   * @param string
   */
  public function setPhase($phase)
  {
    $this->phase = $phase;
  }
  /**
   * @return string
   */
  public function getPhase()
  {
    return $this->phase;
  }
  /**
   * @param ProjectsMetadata
   */
  public function setProjectMetadata(ProjectsMetadata $projectMetadata)
  {
    $this->projectMetadata = $projectMetadata;
  }
  /**
   * @return ProjectsMetadata
   */
  public function getProjectMetadata()
  {
    return $this->projectMetadata;
  }
  /**
   * @param ContainerState
   */
  public function setState(ContainerState $state)
  {
    $this->state = $state;
  }
  /**
   * @return ContainerState
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectEvent::class, 'Google_Service_Appengine_ProjectEvent');
