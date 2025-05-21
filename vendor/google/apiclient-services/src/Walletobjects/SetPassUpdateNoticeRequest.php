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

namespace Google\Service\Walletobjects;

class SetPassUpdateNoticeRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $externalPassId;
  /**
   * @var string
   */
  public $updateUri;
  /**
   * @var string
   */
  public $updatedPassJwtSignature;

  /**
   * @param string
   */
  public function setExternalPassId($externalPassId)
  {
    $this->externalPassId = $externalPassId;
  }
  /**
   * @return string
   */
  public function getExternalPassId()
  {
    return $this->externalPassId;
  }
  /**
   * @param string
   */
  public function setUpdateUri($updateUri)
  {
    $this->updateUri = $updateUri;
  }
  /**
   * @return string
   */
  public function getUpdateUri()
  {
    return $this->updateUri;
  }
  /**
   * @param string
   */
  public function setUpdatedPassJwtSignature($updatedPassJwtSignature)
  {
    $this->updatedPassJwtSignature = $updatedPassJwtSignature;
  }
  /**
   * @return string
   */
  public function getUpdatedPassJwtSignature()
  {
    return $this->updatedPassJwtSignature;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SetPassUpdateNoticeRequest::class, 'Google_Service_Walletobjects_SetPassUpdateNoticeRequest');
