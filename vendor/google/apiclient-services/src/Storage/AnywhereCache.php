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

namespace Google\Service\Storage;

class AnywhereCache extends \Google\Model
{
  /**
   * @var string
   */
  public $admissionPolicy;
  /**
   * @var string
   */
  public $anywhereCacheId;
  /**
   * @var string
   */
  public $bucket;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var bool
   */
  public $pendingUpdate;
  /**
   * @var string
   */
  public $selfLink;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $ttl;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $zone;

  /**
   * @param string
   */
  public function setAdmissionPolicy($admissionPolicy)
  {
    $this->admissionPolicy = $admissionPolicy;
  }
  /**
   * @return string
   */
  public function getAdmissionPolicy()
  {
    return $this->admissionPolicy;
  }
  /**
   * @param string
   */
  public function setAnywhereCacheId($anywhereCacheId)
  {
    $this->anywhereCacheId = $anywhereCacheId;
  }
  /**
   * @return string
   */
  public function getAnywhereCacheId()
  {
    return $this->anywhereCacheId;
  }
  /**
   * @param string
   */
  public function setBucket($bucket)
  {
    $this->bucket = $bucket;
  }
  /**
   * @return string
   */
  public function getBucket()
  {
    return $this->bucket;
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
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
  }
  /**
   * @param bool
   */
  public function setPendingUpdate($pendingUpdate)
  {
    $this->pendingUpdate = $pendingUpdate;
  }
  /**
   * @return bool
   */
  public function getPendingUpdate()
  {
    return $this->pendingUpdate;
  }
  /**
   * @param string
   */
  public function setSelfLink($selfLink)
  {
    $this->selfLink = $selfLink;
  }
  /**
   * @return string
   */
  public function getSelfLink()
  {
    return $this->selfLink;
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
  public function setTtl($ttl)
  {
    $this->ttl = $ttl;
  }
  /**
   * @return string
   */
  public function getTtl()
  {
    return $this->ttl;
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
  /**
   * @param string
   */
  public function setZone($zone)
  {
    $this->zone = $zone;
  }
  /**
   * @return string
   */
  public function getZone()
  {
    return $this->zone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AnywhereCache::class, 'Google_Service_Storage_AnywhereCache');
