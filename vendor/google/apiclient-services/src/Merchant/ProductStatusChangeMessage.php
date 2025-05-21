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

namespace Google\Service\Merchant;

class ProductStatusChangeMessage extends \Google\Collection
{
  protected $collection_key = 'changes';
  /**
   * @var string
   */
  public $account;
  /**
   * @var string
   */
  public $attribute;
  protected $changesType = ProductChange::class;
  protected $changesDataType = 'array';
  /**
   * @var string
   */
  public $eventTime;
  /**
   * @var string
   */
  public $expirationTime;
  /**
   * @var string
   */
  public $managingAccount;
  /**
   * @var string
   */
  public $resource;
  /**
   * @var string
   */
  public $resourceId;
  /**
   * @var string
   */
  public $resourceType;

  /**
   * @param string
   */
  public function setAccount($account)
  {
    $this->account = $account;
  }
  /**
   * @return string
   */
  public function getAccount()
  {
    return $this->account;
  }
  /**
   * @param string
   */
  public function setAttribute($attribute)
  {
    $this->attribute = $attribute;
  }
  /**
   * @return string
   */
  public function getAttribute()
  {
    return $this->attribute;
  }
  /**
   * @param ProductChange[]
   */
  public function setChanges($changes)
  {
    $this->changes = $changes;
  }
  /**
   * @return ProductChange[]
   */
  public function getChanges()
  {
    return $this->changes;
  }
  /**
   * @param string
   */
  public function setEventTime($eventTime)
  {
    $this->eventTime = $eventTime;
  }
  /**
   * @return string
   */
  public function getEventTime()
  {
    return $this->eventTime;
  }
  /**
   * @param string
   */
  public function setExpirationTime($expirationTime)
  {
    $this->expirationTime = $expirationTime;
  }
  /**
   * @return string
   */
  public function getExpirationTime()
  {
    return $this->expirationTime;
  }
  /**
   * @param string
   */
  public function setManagingAccount($managingAccount)
  {
    $this->managingAccount = $managingAccount;
  }
  /**
   * @return string
   */
  public function getManagingAccount()
  {
    return $this->managingAccount;
  }
  /**
   * @param string
   */
  public function setResource($resource)
  {
    $this->resource = $resource;
  }
  /**
   * @return string
   */
  public function getResource()
  {
    return $this->resource;
  }
  /**
   * @param string
   */
  public function setResourceId($resourceId)
  {
    $this->resourceId = $resourceId;
  }
  /**
   * @return string
   */
  public function getResourceId()
  {
    return $this->resourceId;
  }
  /**
   * @param string
   */
  public function setResourceType($resourceType)
  {
    $this->resourceType = $resourceType;
  }
  /**
   * @return string
   */
  public function getResourceType()
  {
    return $this->resourceType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProductStatusChangeMessage::class, 'Google_Service_Merchant_ProductStatusChangeMessage');
