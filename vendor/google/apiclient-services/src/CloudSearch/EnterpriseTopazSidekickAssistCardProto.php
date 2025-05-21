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

namespace Google\Service\CloudSearch;

class EnterpriseTopazSidekickAssistCardProto extends \Google\Model
{
  protected $agendaGroupCardProtoType = EnterpriseTopazSidekickAgendaGroupCardProto::class;
  protected $agendaGroupCardProtoDataType = '';
  protected $cardMetadataType = EnterpriseTopazSidekickCardMetadata::class;
  protected $cardMetadataDataType = '';
  /**
   * @var string
   */
  public $cardType;
  protected $conflictingMeetingsCardType = EnterpriseTopazSidekickConflictingEventsCardProto::class;
  protected $conflictingMeetingsCardDataType = '';
  protected $documentListCardType = EnterpriseTopazSidekickDocumentPerCategoryList::class;
  protected $documentListCardDataType = '';
  protected $documentsWithMentionsType = EnterpriseTopazSidekickDocumentPerCategoryList::class;
  protected $documentsWithMentionsDataType = '';
  protected $findMeetingTimeCardType = EnterpriseTopazSidekickFindMeetingTimeCardProto::class;
  protected $findMeetingTimeCardDataType = '';
  protected $genericAnswerCardType = EnterpriseTopazSidekickGenericAnswerCard::class;
  protected $genericAnswerCardDataType = '';
  protected $getAndKeepAheadCardType = EnterpriseTopazSidekickGetAndKeepAheadCardProto::class;
  protected $getAndKeepAheadCardDataType = '';
  protected $meetingType = EnterpriseTopazSidekickAgendaEntry::class;
  protected $meetingDataType = '';
  protected $meetingNotesCardType = EnterpriseTopazSidekickMeetingNotesCardProto::class;
  protected $meetingNotesCardDataType = '';
  protected $meetingNotesCardRequestType = EnterpriseTopazSidekickMeetingNotesCardRequest::class;
  protected $meetingNotesCardRequestDataType = '';
  protected $peopleDisambiguationCardType = EnterpriseTopazSidekickPeopleDisambiguationCard::class;
  protected $peopleDisambiguationCardDataType = '';
  protected $peoplePromotionCardType = PeoplePromotionCard::class;
  protected $peoplePromotionCardDataType = '';
  protected $personAnswerCardType = EnterpriseTopazSidekickPeopleAnswerPersonAnswerCard::class;
  protected $personAnswerCardDataType = '';
  protected $personProfileCardType = EnterpriseTopazSidekickPersonProfileCard::class;
  protected $personProfileCardDataType = '';
  protected $personalizedDocsCardType = EnterpriseTopazSidekickPersonalizedDocsCardProto::class;
  protected $personalizedDocsCardDataType = '';
  protected $relatedPeopleAnswerCardType = EnterpriseTopazSidekickPeopleAnswerRelatedPeopleAnswerCard::class;
  protected $relatedPeopleAnswerCardDataType = '';
  protected $shareMeetingDocsCardType = EnterpriseTopazSidekickShareMeetingDocsCardProto::class;
  protected $shareMeetingDocsCardDataType = '';
  protected $sharedDocumentsType = EnterpriseTopazSidekickDocumentPerCategoryList::class;
  protected $sharedDocumentsDataType = '';
  protected $suggestedQueryAnswerCardType = EnterpriseTopazSidekickAnswerSuggestedQueryAnswerCard::class;
  protected $suggestedQueryAnswerCardDataType = '';
  protected $thirdPartyAnswerCardType = ThirdPartyGenericCard::class;
  protected $thirdPartyAnswerCardDataType = '';
  protected $workInProgressCardProtoType = EnterpriseTopazSidekickRecentDocumentsCardProto::class;
  protected $workInProgressCardProtoDataType = '';

  /**
   * @param EnterpriseTopazSidekickAgendaGroupCardProto
   */
  public function setAgendaGroupCardProto(EnterpriseTopazSidekickAgendaGroupCardProto $agendaGroupCardProto)
  {
    $this->agendaGroupCardProto = $agendaGroupCardProto;
  }
  /**
   * @return EnterpriseTopazSidekickAgendaGroupCardProto
   */
  public function getAgendaGroupCardProto()
  {
    return $this->agendaGroupCardProto;
  }
  /**
   * @param EnterpriseTopazSidekickCardMetadata
   */
  public function setCardMetadata(EnterpriseTopazSidekickCardMetadata $cardMetadata)
  {
    $this->cardMetadata = $cardMetadata;
  }
  /**
   * @return EnterpriseTopazSidekickCardMetadata
   */
  public function getCardMetadata()
  {
    return $this->cardMetadata;
  }
  /**
   * @param string
   */
  public function setCardType($cardType)
  {
    $this->cardType = $cardType;
  }
  /**
   * @return string
   */
  public function getCardType()
  {
    return $this->cardType;
  }
  /**
   * @param EnterpriseTopazSidekickConflictingEventsCardProto
   */
  public function setConflictingMeetingsCard(EnterpriseTopazSidekickConflictingEventsCardProto $conflictingMeetingsCard)
  {
    $this->conflictingMeetingsCard = $conflictingMeetingsCard;
  }
  /**
   * @return EnterpriseTopazSidekickConflictingEventsCardProto
   */
  public function getConflictingMeetingsCard()
  {
    return $this->conflictingMeetingsCard;
  }
  /**
   * @param EnterpriseTopazSidekickDocumentPerCategoryList
   */
  public function setDocumentListCard(EnterpriseTopazSidekickDocumentPerCategoryList $documentListCard)
  {
    $this->documentListCard = $documentListCard;
  }
  /**
   * @return EnterpriseTopazSidekickDocumentPerCategoryList
   */
  public function getDocumentListCard()
  {
    return $this->documentListCard;
  }
  /**
   * @param EnterpriseTopazSidekickDocumentPerCategoryList
   */
  public function setDocumentsWithMentions(EnterpriseTopazSidekickDocumentPerCategoryList $documentsWithMentions)
  {
    $this->documentsWithMentions = $documentsWithMentions;
  }
  /**
   * @return EnterpriseTopazSidekickDocumentPerCategoryList
   */
  public function getDocumentsWithMentions()
  {
    return $this->documentsWithMentions;
  }
  /**
   * @param EnterpriseTopazSidekickFindMeetingTimeCardProto
   */
  public function setFindMeetingTimeCard(EnterpriseTopazSidekickFindMeetingTimeCardProto $findMeetingTimeCard)
  {
    $this->findMeetingTimeCard = $findMeetingTimeCard;
  }
  /**
   * @return EnterpriseTopazSidekickFindMeetingTimeCardProto
   */
  public function getFindMeetingTimeCard()
  {
    return $this->findMeetingTimeCard;
  }
  /**
   * @param EnterpriseTopazSidekickGenericAnswerCard
   */
  public function setGenericAnswerCard(EnterpriseTopazSidekickGenericAnswerCard $genericAnswerCard)
  {
    $this->genericAnswerCard = $genericAnswerCard;
  }
  /**
   * @return EnterpriseTopazSidekickGenericAnswerCard
   */
  public function getGenericAnswerCard()
  {
    return $this->genericAnswerCard;
  }
  /**
   * @param EnterpriseTopazSidekickGetAndKeepAheadCardProto
   */
  public function setGetAndKeepAheadCard(EnterpriseTopazSidekickGetAndKeepAheadCardProto $getAndKeepAheadCard)
  {
    $this->getAndKeepAheadCard = $getAndKeepAheadCard;
  }
  /**
   * @return EnterpriseTopazSidekickGetAndKeepAheadCardProto
   */
  public function getGetAndKeepAheadCard()
  {
    return $this->getAndKeepAheadCard;
  }
  /**
   * @param EnterpriseTopazSidekickAgendaEntry
   */
  public function setMeeting(EnterpriseTopazSidekickAgendaEntry $meeting)
  {
    $this->meeting = $meeting;
  }
  /**
   * @return EnterpriseTopazSidekickAgendaEntry
   */
  public function getMeeting()
  {
    return $this->meeting;
  }
  /**
   * @param EnterpriseTopazSidekickMeetingNotesCardProto
   */
  public function setMeetingNotesCard(EnterpriseTopazSidekickMeetingNotesCardProto $meetingNotesCard)
  {
    $this->meetingNotesCard = $meetingNotesCard;
  }
  /**
   * @return EnterpriseTopazSidekickMeetingNotesCardProto
   */
  public function getMeetingNotesCard()
  {
    return $this->meetingNotesCard;
  }
  /**
   * @param EnterpriseTopazSidekickMeetingNotesCardRequest
   */
  public function setMeetingNotesCardRequest(EnterpriseTopazSidekickMeetingNotesCardRequest $meetingNotesCardRequest)
  {
    $this->meetingNotesCardRequest = $meetingNotesCardRequest;
  }
  /**
   * @return EnterpriseTopazSidekickMeetingNotesCardRequest
   */
  public function getMeetingNotesCardRequest()
  {
    return $this->meetingNotesCardRequest;
  }
  /**
   * @param EnterpriseTopazSidekickPeopleDisambiguationCard
   */
  public function setPeopleDisambiguationCard(EnterpriseTopazSidekickPeopleDisambiguationCard $peopleDisambiguationCard)
  {
    $this->peopleDisambiguationCard = $peopleDisambiguationCard;
  }
  /**
   * @return EnterpriseTopazSidekickPeopleDisambiguationCard
   */
  public function getPeopleDisambiguationCard()
  {
    return $this->peopleDisambiguationCard;
  }
  /**
   * @param PeoplePromotionCard
   */
  public function setPeoplePromotionCard(PeoplePromotionCard $peoplePromotionCard)
  {
    $this->peoplePromotionCard = $peoplePromotionCard;
  }
  /**
   * @return PeoplePromotionCard
   */
  public function getPeoplePromotionCard()
  {
    return $this->peoplePromotionCard;
  }
  /**
   * @param EnterpriseTopazSidekickPeopleAnswerPersonAnswerCard
   */
  public function setPersonAnswerCard(EnterpriseTopazSidekickPeopleAnswerPersonAnswerCard $personAnswerCard)
  {
    $this->personAnswerCard = $personAnswerCard;
  }
  /**
   * @return EnterpriseTopazSidekickPeopleAnswerPersonAnswerCard
   */
  public function getPersonAnswerCard()
  {
    return $this->personAnswerCard;
  }
  /**
   * @param EnterpriseTopazSidekickPersonProfileCard
   */
  public function setPersonProfileCard(EnterpriseTopazSidekickPersonProfileCard $personProfileCard)
  {
    $this->personProfileCard = $personProfileCard;
  }
  /**
   * @return EnterpriseTopazSidekickPersonProfileCard
   */
  public function getPersonProfileCard()
  {
    return $this->personProfileCard;
  }
  /**
   * @param EnterpriseTopazSidekickPersonalizedDocsCardProto
   */
  public function setPersonalizedDocsCard(EnterpriseTopazSidekickPersonalizedDocsCardProto $personalizedDocsCard)
  {
    $this->personalizedDocsCard = $personalizedDocsCard;
  }
  /**
   * @return EnterpriseTopazSidekickPersonalizedDocsCardProto
   */
  public function getPersonalizedDocsCard()
  {
    return $this->personalizedDocsCard;
  }
  /**
   * @param EnterpriseTopazSidekickPeopleAnswerRelatedPeopleAnswerCard
   */
  public function setRelatedPeopleAnswerCard(EnterpriseTopazSidekickPeopleAnswerRelatedPeopleAnswerCard $relatedPeopleAnswerCard)
  {
    $this->relatedPeopleAnswerCard = $relatedPeopleAnswerCard;
  }
  /**
   * @return EnterpriseTopazSidekickPeopleAnswerRelatedPeopleAnswerCard
   */
  public function getRelatedPeopleAnswerCard()
  {
    return $this->relatedPeopleAnswerCard;
  }
  /**
   * @param EnterpriseTopazSidekickShareMeetingDocsCardProto
   */
  public function setShareMeetingDocsCard(EnterpriseTopazSidekickShareMeetingDocsCardProto $shareMeetingDocsCard)
  {
    $this->shareMeetingDocsCard = $shareMeetingDocsCard;
  }
  /**
   * @return EnterpriseTopazSidekickShareMeetingDocsCardProto
   */
  public function getShareMeetingDocsCard()
  {
    return $this->shareMeetingDocsCard;
  }
  /**
   * @param EnterpriseTopazSidekickDocumentPerCategoryList
   */
  public function setSharedDocuments(EnterpriseTopazSidekickDocumentPerCategoryList $sharedDocuments)
  {
    $this->sharedDocuments = $sharedDocuments;
  }
  /**
   * @return EnterpriseTopazSidekickDocumentPerCategoryList
   */
  public function getSharedDocuments()
  {
    return $this->sharedDocuments;
  }
  /**
   * @param EnterpriseTopazSidekickAnswerSuggestedQueryAnswerCard
   */
  public function setSuggestedQueryAnswerCard(EnterpriseTopazSidekickAnswerSuggestedQueryAnswerCard $suggestedQueryAnswerCard)
  {
    $this->suggestedQueryAnswerCard = $suggestedQueryAnswerCard;
  }
  /**
   * @return EnterpriseTopazSidekickAnswerSuggestedQueryAnswerCard
   */
  public function getSuggestedQueryAnswerCard()
  {
    return $this->suggestedQueryAnswerCard;
  }
  /**
   * @param ThirdPartyGenericCard
   */
  public function setThirdPartyAnswerCard(ThirdPartyGenericCard $thirdPartyAnswerCard)
  {
    $this->thirdPartyAnswerCard = $thirdPartyAnswerCard;
  }
  /**
   * @return ThirdPartyGenericCard
   */
  public function getThirdPartyAnswerCard()
  {
    return $this->thirdPartyAnswerCard;
  }
  /**
   * @param EnterpriseTopazSidekickRecentDocumentsCardProto
   */
  public function setWorkInProgressCardProto(EnterpriseTopazSidekickRecentDocumentsCardProto $workInProgressCardProto)
  {
    $this->workInProgressCardProto = $workInProgressCardProto;
  }
  /**
   * @return EnterpriseTopazSidekickRecentDocumentsCardProto
   */
  public function getWorkInProgressCardProto()
  {
    return $this->workInProgressCardProto;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnterpriseTopazSidekickAssistCardProto::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickAssistCardProto');
