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

namespace Google\Service\WorkspaceEvents;

class Subscription extends \Google\Collection
{
  protected $collection_key = 'eventTypes';
  /**
   * @var string
   */
  public $authority;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string[]
   */
  public $eventTypes;
  /**
   * @var string
   */
  public $expireTime;
  /**
   * @var string
   */
  public $name;
  protected $notificationEndpointType = NotificationEndpoint::class;
  protected $notificationEndpointDataType = '';
  protected $payloadOptionsType = PayloadOptions::class;
  protected $payloadOptionsDataType = '';
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
  public $suspensionReason;
  /**
   * @var string
   */
  public $targetResource;
  /**
   * @var string
   */
  public $ttl;
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setAuthority($authority)
  {
    $this->authority = $authority;
  }
  /**
   * @return string
   */
  public function getAuthority()
  {
    return $this->authority;
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
  public function setEventTypes($eventTypes)
  {
    $this->eventTypes = $eventTypes;
  }
  /**
   * @return string[]
   */
  public function getEventTypes()
  {
    return $this->eventTypes;
  }
  /**
   * @param string
   */
  public function setExpireTime($expireTime)
  {
    $this->expireTime = $expireTime;
  }
  /**
   * @return string
   */
  public function getExpireTime()
  {
    return $this->expireTime;
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
   * @param NotificationEndpoint
   */
  public function setNotificationEndpoint(NotificationEndpoint $notificationEndpoint)
  {
    $this->notificationEndpoint = $notificationEndpoint;
  }
  /**
   * @return NotificationEndpoint
   */
  public function getNotificationEndpoint()
  {
    return $this->notificationEndpoint;
  }
  /**
   * @param PayloadOptions
   */
  public function setPayloadOptions(PayloadOptions $payloadOptions)
  {
    $this->payloadOptions = $payloadOptions;
  }
  /**
   * @return PayloadOptions
   */
  public function getPayloadOptions()
  {
    return $this->payloadOptions;
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
  public function setSuspensionReason($suspensionReason)
  {
    $this->suspensionReason = $suspensionReason;
  }
  /**
   * @return string
   */
  public function getSuspensionReason()
  {
    return $this->suspensionReason;
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
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
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
class_alias(Subscription::class, 'Google_Service_WorkspaceEvents_Subscription');
