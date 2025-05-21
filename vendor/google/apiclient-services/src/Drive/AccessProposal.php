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

namespace Google\Service\Drive;

class AccessProposal extends \Google\Collection
{
  protected $collection_key = 'rolesAndViews';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $fileId;
  /**
   * @var string
   */
  public $proposalId;
  /**
   * @var string
   */
  public $recipientEmailAddress;
  /**
   * @var string
   */
  public $requestMessage;
  /**
   * @var string
   */
  public $requesterEmailAddress;
  protected $rolesAndViewsType = AccessProposalRoleAndView::class;
  protected $rolesAndViewsDataType = 'array';

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
  public function setFileId($fileId)
  {
    $this->fileId = $fileId;
  }
  /**
   * @return string
   */
  public function getFileId()
  {
    return $this->fileId;
  }
  /**
   * @param string
   */
  public function setProposalId($proposalId)
  {
    $this->proposalId = $proposalId;
  }
  /**
   * @return string
   */
  public function getProposalId()
  {
    return $this->proposalId;
  }
  /**
   * @param string
   */
  public function setRecipientEmailAddress($recipientEmailAddress)
  {
    $this->recipientEmailAddress = $recipientEmailAddress;
  }
  /**
   * @return string
   */
  public function getRecipientEmailAddress()
  {
    return $this->recipientEmailAddress;
  }
  /**
   * @param string
   */
  public function setRequestMessage($requestMessage)
  {
    $this->requestMessage = $requestMessage;
  }
  /**
   * @return string
   */
  public function getRequestMessage()
  {
    return $this->requestMessage;
  }
  /**
   * @param string
   */
  public function setRequesterEmailAddress($requesterEmailAddress)
  {
    $this->requesterEmailAddress = $requesterEmailAddress;
  }
  /**
   * @return string
   */
  public function getRequesterEmailAddress()
  {
    return $this->requesterEmailAddress;
  }
  /**
   * @param AccessProposalRoleAndView[]
   */
  public function setRolesAndViews($rolesAndViews)
  {
    $this->rolesAndViews = $rolesAndViews;
  }
  /**
   * @return AccessProposalRoleAndView[]
   */
  public function getRolesAndViews()
  {
    return $this->rolesAndViews;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AccessProposal::class, 'Google_Service_Drive_AccessProposal');
