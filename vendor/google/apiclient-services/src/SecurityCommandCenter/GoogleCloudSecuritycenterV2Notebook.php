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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV2Notebook extends \Google\Model
{
  /**
   * @var string
   */
  public $lastAuthor;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $notebookUpdateTime;
  /**
   * @var string
   */
  public $service;

  /**
   * @param string
   */
  public function setLastAuthor($lastAuthor)
  {
    $this->lastAuthor = $lastAuthor;
  }
  /**
   * @return string
   */
  public function getLastAuthor()
  {
    return $this->lastAuthor;
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
  public function setNotebookUpdateTime($notebookUpdateTime)
  {
    $this->notebookUpdateTime = $notebookUpdateTime;
  }
  /**
   * @return string
   */
  public function getNotebookUpdateTime()
  {
    return $this->notebookUpdateTime;
  }
  /**
   * @param string
   */
  public function setService($service)
  {
    $this->service = $service;
  }
  /**
   * @return string
   */
  public function getService()
  {
    return $this->service;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2Notebook::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2Notebook');
