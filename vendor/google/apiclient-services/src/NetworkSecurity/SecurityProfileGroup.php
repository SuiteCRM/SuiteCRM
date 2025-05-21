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

namespace Google\Service\NetworkSecurity;

class SecurityProfileGroup extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $customInterceptProfile;
  /**
   * @var string
   */
  public $customMirroringProfile;
  /**
   * @var string
   */
  public $dataPathId;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $threatPreventionProfile;
  /**
   * @var string
   */
  public $updateTime;

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
  public function setCustomInterceptProfile($customInterceptProfile)
  {
    $this->customInterceptProfile = $customInterceptProfile;
  }
  /**
   * @return string
   */
  public function getCustomInterceptProfile()
  {
    return $this->customInterceptProfile;
  }
  /**
   * @param string
   */
  public function setCustomMirroringProfile($customMirroringProfile)
  {
    $this->customMirroringProfile = $customMirroringProfile;
  }
  /**
   * @return string
   */
  public function getCustomMirroringProfile()
  {
    return $this->customMirroringProfile;
  }
  /**
   * @param string
   */
  public function setDataPathId($dataPathId)
  {
    $this->dataPathId = $dataPathId;
  }
  /**
   * @return string
   */
  public function getDataPathId()
  {
    return $this->dataPathId;
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
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
  public function setThreatPreventionProfile($threatPreventionProfile)
  {
    $this->threatPreventionProfile = $threatPreventionProfile;
  }
  /**
   * @return string
   */
  public function getThreatPreventionProfile()
  {
    return $this->threatPreventionProfile;
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
class_alias(SecurityProfileGroup::class, 'Google_Service_NetworkSecurity_SecurityProfileGroup');
