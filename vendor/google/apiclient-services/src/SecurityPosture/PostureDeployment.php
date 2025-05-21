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

namespace Google\Service\SecurityPosture;

class PostureDeployment extends \Google\Collection
{
  protected $collection_key = 'categories';
  /**
   * @var string[]
   */
  public $annotations;
  /**
   * @var string[]
   */
  public $categories;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $desiredPostureId;
  /**
   * @var string
   */
  public $desiredPostureRevisionId;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string
   */
  public $failureMessage;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $postureId;
  /**
   * @var string
   */
  public $postureRevisionId;
  /**
   * @var bool
   */
  public $reconciling;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $targetResource;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string[]
   */
  public function setAnnotations($annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return string[]
   */
  public function getAnnotations()
  {
    return $this->annotations;
  }
  /**
   * @param string[]
   */
  public function setCategories($categories)
  {
    $this->categories = $categories;
  }
  /**
   * @return string[]
   */
  public function getCategories()
  {
    return $this->categories;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
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
  public function setDesiredPostureId($desiredPostureId)
  {
    $this->desiredPostureId = $desiredPostureId;
  }
  /**
   * @return string
   */
  public function getDesiredPostureId()
  {
    return $this->desiredPostureId;
  }
  /**
   * @param string
   */
  public function setDesiredPostureRevisionId($desiredPostureRevisionId)
  {
    $this->desiredPostureRevisionId = $desiredPostureRevisionId;
  }
  /**
   * @return string
   */
  public function getDesiredPostureRevisionId()
  {
    return $this->desiredPostureRevisionId;
  }
  /**
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
  }
  /**
   * @param string
   */
  public function setFailureMessage($failureMessage)
  {
    $this->failureMessage = $failureMessage;
  }
  /**
   * @return string
   */
  public function getFailureMessage()
  {
    return $this->failureMessage;
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
  public function setPostureId($postureId)
  {
    $this->postureId = $postureId;
  }
  /**
   * @return string
   */
  public function getPostureId()
  {
    return $this->postureId;
  }
  /**
   * @param string
   */
  public function setPostureRevisionId($postureRevisionId)
  {
    $this->postureRevisionId = $postureRevisionId;
  }
  /**
   * @return string
   */
  public function getPostureRevisionId()
  {
    return $this->postureRevisionId;
  }
  /**
   * @param bool
   */
  public function setReconciling($reconciling)
  {
    $this->reconciling = $reconciling;
  }
  /**
   * @return bool
   */
  public function getReconciling()
  {
    return $this->reconciling;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setTargetResource($targetResource)
  {
    $this->targetResource = $targetResource;
  }
  /**
   * @return string
   */
  public function getTargetResource()
  {
    return $this->targetResource;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PostureDeployment::class, 'Google_Service_SecurityPosture_PostureDeployment');
