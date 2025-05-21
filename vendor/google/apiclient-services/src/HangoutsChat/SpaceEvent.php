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

namespace Google\Service\HangoutsChat;

class SpaceEvent extends \Google\Model
{
  /**
   * @var string
   */
  public $eventTime;
  /**
   * @var string
   */
  public $eventType;
  protected $membershipBatchCreatedEventDataType = MembershipBatchCreatedEventData::class;
  protected $membershipBatchCreatedEventDataDataType = '';
  protected $membershipBatchDeletedEventDataType = MembershipBatchDeletedEventData::class;
  protected $membershipBatchDeletedEventDataDataType = '';
  protected $membershipBatchUpdatedEventDataType = MembershipBatchUpdatedEventData::class;
  protected $membershipBatchUpdatedEventDataDataType = '';
  protected $membershipCreatedEventDataType = MembershipCreatedEventData::class;
  protected $membershipCreatedEventDataDataType = '';
  protected $membershipDeletedEventDataType = MembershipDeletedEventData::class;
  protected $membershipDeletedEventDataDataType = '';
  protected $membershipUpdatedEventDataType = MembershipUpdatedEventData::class;
  protected $membershipUpdatedEventDataDataType = '';
  protected $messageBatchCreatedEventDataType = MessageBatchCreatedEventData::class;
  protected $messageBatchCreatedEventDataDataType = '';
  protected $messageBatchDeletedEventDataType = MessageBatchDeletedEventData::class;
  protected $messageBatchDeletedEventDataDataType = '';
  protected $messageBatchUpdatedEventDataType = MessageBatchUpdatedEventData::class;
  protected $messageBatchUpdatedEventDataDataType = '';
  protected $messageCreatedEventDataType = MessageCreatedEventData::class;
  protected $messageCreatedEventDataDataType = '';
  protected $messageDeletedEventDataType = MessageDeletedEventData::class;
  protected $messageDeletedEventDataDataType = '';
  protected $messageUpdatedEventDataType = MessageUpdatedEventData::class;
  protected $messageUpdatedEventDataDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $reactionBatchCreatedEventDataType = ReactionBatchCreatedEventData::class;
  protected $reactionBatchCreatedEventDataDataType = '';
  protected $reactionBatchDeletedEventDataType = ReactionBatchDeletedEventData::class;
  protected $reactionBatchDeletedEventDataDataType = '';
  protected $reactionCreatedEventDataType = ReactionCreatedEventData::class;
  protected $reactionCreatedEventDataDataType = '';
  protected $reactionDeletedEventDataType = ReactionDeletedEventData::class;
  protected $reactionDeletedEventDataDataType = '';
  protected $spaceBatchUpdatedEventDataType = SpaceBatchUpdatedEventData::class;
  protected $spaceBatchUpdatedEventDataDataType = '';
  protected $spaceUpdatedEventDataType = SpaceUpdatedEventData::class;
  protected $spaceUpdatedEventDataDataType = '';

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
  public function setEventType($eventType)
  {
    $this->eventType = $eventType;
  }
  /**
   * @return string
   */
  public function getEventType()
  {
    return $this->eventType;
  }
  /**
   * @param MembershipBatchCreatedEventData
   */
  public function setMembershipBatchCreatedEventData(MembershipBatchCreatedEventData $membershipBatchCreatedEventData)
  {
    $this->membershipBatchCreatedEventData = $membershipBatchCreatedEventData;
  }
  /**
   * @return MembershipBatchCreatedEventData
   */
  public function getMembershipBatchCreatedEventData()
  {
    return $this->membershipBatchCreatedEventData;
  }
  /**
   * @param MembershipBatchDeletedEventData
   */
  public function setMembershipBatchDeletedEventData(MembershipBatchDeletedEventData $membershipBatchDeletedEventData)
  {
    $this->membershipBatchDeletedEventData = $membershipBatchDeletedEventData;
  }
  /**
   * @return MembershipBatchDeletedEventData
   */
  public function getMembershipBatchDeletedEventData()
  {
    return $this->membershipBatchDeletedEventData;
  }
  /**
   * @param MembershipBatchUpdatedEventData
   */
  public function setMembershipBatchUpdatedEventData(MembershipBatchUpdatedEventData $membershipBatchUpdatedEventData)
  {
    $this->membershipBatchUpdatedEventData = $membershipBatchUpdatedEventData;
  }
  /**
   * @return MembershipBatchUpdatedEventData
   */
  public function getMembershipBatchUpdatedEventData()
  {
    return $this->membershipBatchUpdatedEventData;
  }
  /**
   * @param MembershipCreatedEventData
   */
  public function setMembershipCreatedEventData(MembershipCreatedEventData $membershipCreatedEventData)
  {
    $this->membershipCreatedEventData = $membershipCreatedEventData;
  }
  /**
   * @return MembershipCreatedEventData
   */
  public function getMembershipCreatedEventData()
  {
    return $this->membershipCreatedEventData;
  }
  /**
   * @param MembershipDeletedEventData
   */
  public function setMembershipDeletedEventData(MembershipDeletedEventData $membershipDeletedEventData)
  {
    $this->membershipDeletedEventData = $membershipDeletedEventData;
  }
  /**
   * @return MembershipDeletedEventData
   */
  public function getMembershipDeletedEventData()
  {
    return $this->membershipDeletedEventData;
  }
  /**
   * @param MembershipUpdatedEventData
   */
  public function setMembershipUpdatedEventData(MembershipUpdatedEventData $membershipUpdatedEventData)
  {
    $this->membershipUpdatedEventData = $membershipUpdatedEventData;
  }
  /**
   * @return MembershipUpdatedEventData
   */
  public function getMembershipUpdatedEventData()
  {
    return $this->membershipUpdatedEventData;
  }
  /**
   * @param MessageBatchCreatedEventData
   */
  public function setMessageBatchCreatedEventData(MessageBatchCreatedEventData $messageBatchCreatedEventData)
  {
    $this->messageBatchCreatedEventData = $messageBatchCreatedEventData;
  }
  /**
   * @return MessageBatchCreatedEventData
   */
  public function getMessageBatchCreatedEventData()
  {
    return $this->messageBatchCreatedEventData;
  }
  /**
   * @param MessageBatchDeletedEventData
   */
  public function setMessageBatchDeletedEventData(MessageBatchDeletedEventData $messageBatchDeletedEventData)
  {
    $this->messageBatchDeletedEventData = $messageBatchDeletedEventData;
  }
  /**
   * @return MessageBatchDeletedEventData
   */
  public function getMessageBatchDeletedEventData()
  {
    return $this->messageBatchDeletedEventData;
  }
  /**
   * @param MessageBatchUpdatedEventData
   */
  public function setMessageBatchUpdatedEventData(MessageBatchUpdatedEventData $messageBatchUpdatedEventData)
  {
    $this->messageBatchUpdatedEventData = $messageBatchUpdatedEventData;
  }
  /**
   * @return MessageBatchUpdatedEventData
   */
  public function getMessageBatchUpdatedEventData()
  {
    return $this->messageBatchUpdatedEventData;
  }
  /**
   * @param MessageCreatedEventData
   */
  public function setMessageCreatedEventData(MessageCreatedEventData $messageCreatedEventData)
  {
    $this->messageCreatedEventData = $messageCreatedEventData;
  }
  /**
   * @return MessageCreatedEventData
   */
  public function getMessageCreatedEventData()
  {
    return $this->messageCreatedEventData;
  }
  /**
   * @param MessageDeletedEventData
   */
  public function setMessageDeletedEventData(MessageDeletedEventData $messageDeletedEventData)
  {
    $this->messageDeletedEventData = $messageDeletedEventData;
  }
  /**
   * @return MessageDeletedEventData
   */
  public function getMessageDeletedEventData()
  {
    return $this->messageDeletedEventData;
  }
  /**
   * @param MessageUpdatedEventData
   */
  public function setMessageUpdatedEventData(MessageUpdatedEventData $messageUpdatedEventData)
  {
    $this->messageUpdatedEventData = $messageUpdatedEventData;
  }
  /**
   * @return MessageUpdatedEventData
   */
  public function getMessageUpdatedEventData()
  {
    return $this->messageUpdatedEventData;
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
   * @param ReactionBatchCreatedEventData
   */
  public function setReactionBatchCreatedEventData(ReactionBatchCreatedEventData $reactionBatchCreatedEventData)
  {
    $this->reactionBatchCreatedEventData = $reactionBatchCreatedEventData;
  }
  /**
   * @return ReactionBatchCreatedEventData
   */
  public function getReactionBatchCreatedEventData()
  {
    return $this->reactionBatchCreatedEventData;
  }
  /**
   * @param ReactionBatchDeletedEventData
   */
  public function setReactionBatchDeletedEventData(ReactionBatchDeletedEventData $reactionBatchDeletedEventData)
  {
    $this->reactionBatchDeletedEventData = $reactionBatchDeletedEventData;
  }
  /**
   * @return ReactionBatchDeletedEventData
   */
  public function getReactionBatchDeletedEventData()
  {
    return $this->reactionBatchDeletedEventData;
  }
  /**
   * @param ReactionCreatedEventData
   */
  public function setReactionCreatedEventData(ReactionCreatedEventData $reactionCreatedEventData)
  {
    $this->reactionCreatedEventData = $reactionCreatedEventData;
  }
  /**
   * @return ReactionCreatedEventData
   */
  public function getReactionCreatedEventData()
  {
    return $this->reactionCreatedEventData;
  }
  /**
   * @param ReactionDeletedEventData
   */
  public function setReactionDeletedEventData(ReactionDeletedEventData $reactionDeletedEventData)
  {
    $this->reactionDeletedEventData = $reactionDeletedEventData;
  }
  /**
   * @return ReactionDeletedEventData
   */
  public function getReactionDeletedEventData()
  {
    return $this->reactionDeletedEventData;
  }
  /**
   * @param SpaceBatchUpdatedEventData
   */
  public function setSpaceBatchUpdatedEventData(SpaceBatchUpdatedEventData $spaceBatchUpdatedEventData)
  {
    $this->spaceBatchUpdatedEventData = $spaceBatchUpdatedEventData;
  }
  /**
   * @return SpaceBatchUpdatedEventData
   */
  public function getSpaceBatchUpdatedEventData()
  {
    return $this->spaceBatchUpdatedEventData;
  }
  /**
   * @param SpaceUpdatedEventData
   */
  public function setSpaceUpdatedEventData(SpaceUpdatedEventData $spaceUpdatedEventData)
  {
    $this->spaceUpdatedEventData = $spaceUpdatedEventData;
  }
  /**
   * @return SpaceUpdatedEventData
   */
  public function getSpaceUpdatedEventData()
  {
    return $this->spaceUpdatedEventData;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SpaceEvent::class, 'Google_Service_HangoutsChat_SpaceEvent');
